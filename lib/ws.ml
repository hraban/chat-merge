(* Pull-style wrapper around httpun-ws-eio's callback API: incoming messages
   are reassembled from their frames and queued, so callers can just [recv]. *)

type event =
  | Connected of Httpun_ws.Wsd.t
  | Message of string
  | Closed of string

type t = {
  client : Httpun_ws_eio.Client.t;
  wsd : Httpun_ws.Wsd.t;
  events : event Eio.Stream.t;
}

let error_to_string : Httpun_ws.Client_connection.error -> string = function
  | `Handshake_failure (rsp, _body) ->
      Format.asprintf "handshake failed: %a" Httpun.Response.pp_hum rsp
  | `Malformed_response msg -> "malformed response: " ^ msg
  | `Invalid_response_body_length _ -> "invalid response body length"
  | `Exn exn -> Printexc.to_string exn

let websocket_handler events wsd =
  Eio.Stream.add events (Connected wsd);
  let message = Buffer.create 4096 in
  let frame ~opcode ~is_fin ~len:_ payload =
    (* Every payload must be read to the end, or later frames never arrive. *)
    let rec read_into buf ~on_eof =
      Httpun_ws.Payload.schedule_read payload ~on_eof ~on_read:(fun bs ~off ~len ->
          Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
          read_into buf ~on_eof)
    in
    let discard () = read_into (Buffer.create 0) ~on_eof:ignore in
    match (opcode : Httpun_ws.Websocket.Opcode.t) with
    | `Text | `Binary | `Continuation ->
        read_into message ~on_eof:(fun () ->
            if is_fin then (
              Eio.Stream.add events (Message (Buffer.contents message));
              Buffer.clear message))
    | `Ping ->
        discard ();
        Httpun_ws.Wsd.send_pong wsd
    | `Connection_close ->
        discard ();
        Httpun_ws.Wsd.close wsd
    | `Pong | `Other _ -> discard ()
  in
  let eof ?error () =
    let reason =
      match error with
      | Some (`Exn exn) -> Printexc.to_string exn
      | None -> "closed by server"
    in
    Eio.Stream.add events (Closed reason)
  in
  { Httpun_ws.Websocket_connection.frame; eof }

(* Performs the HTTP upgrade on [socket]; returns once the WebSocket is open. *)
let connect ~sw ~host ~port socket =
  let events = Eio.Stream.create 1024 in
  let nonce = String.init 16 (fun _ -> Char.chr (Random.int 256)) in
  let client =
    Httpun_ws_eio.Client.connect ~sw ~nonce ~host ~port ~resource:"/"
      ~error_handler:(fun e -> Eio.Stream.add events (Closed (error_to_string e)))
      ~websocket_handler:(websocket_handler events)
      socket
  in
  match Eio.Stream.take events with
  | Connected wsd -> { client; wsd; events }
  | Closed reason -> failwith reason
  | Message _ -> assert false

let rec recv t =
  match Eio.Stream.take t.events with
  | Message s -> `Message s
  | Closed reason -> `Closed reason
  | Connected _ -> recv t

let send_text t s =
  (* send_bytes masks the buffer in place, so it must be a private copy. *)
  let b = Bytes.of_string s in
  Httpun_ws.Wsd.send_bytes t.wsd ~kind:`Text b ~off:0 ~len:(Bytes.length b)

let close t = Eio.Promise.await (Httpun_ws_eio.Client.shutdown t.client)
