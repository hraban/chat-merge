(* Anonymous Twitch chat reader. Connects to Twitch's WebSocket chat endpoint
   as a "justinfan" guest and prints every chat message to stdout. *)

open Twitch_chat

let host = "irc-ws.chat.twitch.tv"

(* Chat text is attacker-controlled, so printed lines go through these
   filters. Replaced characters are shown as ⟨U+XXXX⟩, which is hard to
   confuse with someone literally typing "\x1b". *)

let add_marker b cp = Printf.bprintf b "⟨U+%04X⟩" cp

(* Characters that can drive the terminal: C0 controls (incl. ESC), DEL, and
   C1 controls (U+0080..U+009F, UTF-8 encoded as C2 80..C2 9F), which some
   terminals also interpret. *)
let sanitize_controls s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      match Char.code s.[i] with
      | c when c < 0x20 || c = 0x7f ->
          add_marker b c;
          go (i + 1)
      | 0xc2 when i + 1 < n && Char.code s.[i + 1] land 0xe0 = 0x80 ->
          add_marker b (Char.code s.[i + 1]);
          go (i + 2)
      | _ ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* Characters that can make a message look like something else: line and
   paragraph separators (fake extra lines) and bidi embeddings, overrides and
   isolates (reordered text). All are 3-byte UTF-8 sequences starting E2. *)
let sanitize_spoofing s =
  let is_spoofing cp =
    cp = 0x2028 || cp = 0x2029
    || (cp >= 0x202A && cp <= 0x202E)
    || (cp >= 0x2066 && cp <= 0x2069)
  in
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i + 2 < n && s.[i] = '\xe2' then
      let cp =
        0x2000
        lor ((Char.code s.[i + 1] land 0x3f) lsl 6)
        lor (Char.code s.[i + 2] land 0x3f)
      in
      if is_spoofing cp then (
        add_marker b cp;
        go (i + 3))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
    else if i < n then (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Applied to every printed line, in order. Remove an entry to disable it. *)
let output_filters = [ sanitize_controls; sanitize_spoofing ]

let print_message out (msg : Irc.message) =
  match (msg.command, msg.params) with
  | "PRIVMSG", [ channel; text ] ->
      let name =
        match List.assoc_opt "display-name" msg.tags with
        | Some n when n <> "" -> n
        | _ -> Option.value (Irc.nick msg) ~default:"?"
      in
      let line =
        (* /me messages arrive as CTCP ACTION. *)
        if String.starts_with ~prefix:"\001ACTION " text then
          let len = String.length text in
          let body = String.sub text 8 (len - 8) in
          let body =
            if String.ends_with ~suffix:"\001" body then
              String.sub body 0 (String.length body - 1)
            else body
          in
          Printf.sprintf "%s * %s %s" channel name body
        else Printf.sprintf "%s <%s> %s" channel name text
      in
      let line =
        List.fold_left (fun l filter -> filter l) line output_filters ^ "\n"
      in
      Eio.Flow.copy_string line out
  | _ -> ()

let handle_line ws out line =
  let line =
    if String.ends_with ~suffix:"\r" line then
      String.sub line 0 (String.length line - 1)
    else line
  in
  match Irc.parse line with
  | Some { command = "PING"; params; _ } ->
      let token = match List.rev params with t :: _ -> t | [] -> host in
      Ws.send_text ws ("PONG :" ^ token)
  | Some msg -> print_message out msg
  | None -> ()

(* One connection's lifetime. Returns when the server closes the socket,
   which it also does shortly after sending RECONNECT. *)
let run_session ~net ~tls_config ~out channels =
  Eio.Net.with_tcp_connect net ~host ~service:"443" @@ fun sock ->
  let tls =
    Tls_eio.client_of_flow tls_config
      ~host:(Domain_name.host_exn (Domain_name.of_string_exn host))
      sock
  in
  let ws =
    Ws.handshake ~host ~path:"/" (tls :> Eio.Flow.two_way_ty Eio.Resource.t)
  in
  let guest_id = String.get_uint16_be (Mirage_crypto_rng.generate 2) 0 in
  Ws.send_text ws "CAP REQ :twitch.tv/tags twitch.tv/commands";
  Ws.send_text ws (Printf.sprintf "NICK justinfan%d" guest_id);
  Ws.send_text ws
    ("JOIN " ^ String.concat "," (List.map (fun c -> "#" ^ c) channels));
  let rec loop () =
    match Ws.recv ws with
    | None -> ()
    | Some data ->
        List.iter (handle_line ws out) (String.split_on_char '\n' data);
        loop ()
  in
  loop ()

let normalize_channel c =
  let c = String.lowercase_ascii c in
  if String.starts_with ~prefix:"#" c then
    String.sub c 1 (String.length c - 1)
  else c

let ok_or_fail = function Ok x -> x | Error (`Msg m) -> failwith m

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
      Eio.Flow.copy_string "usage: twitch-chat CHANNEL...\n"
        (Eio.Stdenv.stderr env);
      exit 2
  | channels ->
      let channels = List.map normalize_channel channels in
      let authenticator = ok_or_fail (Ca_certs.authenticator ()) in
      let tls_config = ok_or_fail (Tls.Config.client ~authenticator ()) in
      let net = Eio.Stdenv.net env and out = Eio.Stdenv.stdout env in
      let rec forever () =
        (try run_session ~net ~tls_config ~out channels with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e -> Eio.traceln "connection error: %s" (Printexc.to_string e));
        Eio.traceln "disconnected, reconnecting in 2s";
        Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;
        forever ()
      in
      forever ()
