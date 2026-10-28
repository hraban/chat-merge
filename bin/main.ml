(* Anonymous Twitch chat reader. Connects to Twitch's WebSocket chat endpoint
   as a "justinfan" guest and prints every chat message to stdout. *)

open Twitch_chat

let host = "irc-ws.chat.twitch.tv"

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
      let line = Sanitize.(apply_filters line output_filters) ^ "\n" in
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
