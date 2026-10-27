(* Minimal RFC 6455 WebSocket client: just enough to talk to Twitch's chat
   endpoint. No extensions, no Sec-WebSocket-Accept verification. *)

module R = Eio.Buf_read

type t = {
  flow : Eio.Flow.two_way_ty Eio.Resource.t;
  buf : R.t;
}

exception Protocol_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Protocol_error s)) fmt

let handshake ~host ~path flow =
  let key = Base64.encode_string (Mirage_crypto_rng.generate 16) in
  let request =
    String.concat "\r\n"
      [
        Printf.sprintf "GET %s HTTP/1.1" path;
        "Host: " ^ host;
        "Upgrade: websocket";
        "Connection: Upgrade";
        "Sec-WebSocket-Key: " ^ key;
        "Sec-WebSocket-Version: 13";
        "";
        "";
      ]
  in
  Eio.Flow.copy_string request flow;
  let buf = R.of_flow flow ~max_size:(1 lsl 20) in
  let status = R.line buf in
  (match String.split_on_char ' ' status with
  | _ :: "101" :: _ -> ()
  | _ -> fail "handshake rejected: %s" status);
  (* Skip the response headers. *)
  let rec skip_headers () = if R.line buf <> "" then skip_headers () in
  skip_headers ();
  { flow; buf }

let xor_mask mask payload =
  String.mapi
    (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
    payload

(* Client-to-server frames must always be masked. *)
let send_frame t ~opcode payload =
  let len = String.length payload in
  let frame = Buffer.create (len + 14) in
  Buffer.add_uint8 frame (0x80 lor opcode);
  if len < 126 then Buffer.add_uint8 frame (0x80 lor len)
  else if len < 0x10000 then (
    Buffer.add_uint8 frame (0x80 lor 126);
    Buffer.add_uint16_be frame len)
  else (
    Buffer.add_uint8 frame (0x80 lor 127);
    Buffer.add_int64_be frame (Int64.of_int len));
  let mask = Mirage_crypto_rng.generate 4 in
  Buffer.add_string frame mask;
  Buffer.add_string frame (xor_mask mask payload);
  Eio.Flow.copy_string (Buffer.contents frame) t.flow

let send_text t s = send_frame t ~opcode:0x1 s

let read_frame t =
  let b0 = R.uint8 t.buf in
  let b1 = R.uint8 t.buf in
  let len =
    match b1 land 0x7f with
    | 126 -> R.BE.uint16 t.buf
    | 127 -> Int64.to_int (R.BE.uint64 t.buf)
    | n -> n
  in
  let mask = if b1 land 0x80 <> 0 then Some (R.take 4 t.buf) else None in
  let payload = R.take len t.buf in
  let payload =
    match mask with None -> payload | Some m -> xor_mask m payload
  in
  (b0 land 0x80 <> 0, b0 land 0x0f, payload)

(* Next complete data message, answering pings along the way.
   [None] once the server sends a close frame. *)
let recv t =
  let rec collect acc =
    let fin, opcode, payload = read_frame t in
    match opcode with
    | 0x0 | 0x1 | 0x2 ->
        let acc = payload :: acc in
        if fin then Some (String.concat "" (List.rev acc)) else collect acc
    | 0x8 -> None
    | 0x9 ->
        send_frame t ~opcode:0xA payload;
        collect acc
    | 0xA -> collect acc
    | op -> fail "unexpected opcode %d" op
  in
  collect []
