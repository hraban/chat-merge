(* Anonymous Twitch chat reader. Connects to Twitch's WebSocket chat endpoint
   as a "justinfan" guest and prints every chat message to stdout. *)

open! Core

let host = "irc-ws.chat.twitch.tv"

let print_message out (msg : Irc.message) =
  match (msg.command, msg.params) with
  | "PRIVMSG", [ channel; text ] ->
      let name =
        match List.Assoc.find msg.tags ~equal:String.equal "display-name" with
        | Some n when not (String.is_empty n) -> n
        | _ -> Option.value (Irc.nick msg) ~default:"?"
      in
      let line =
        (* /me messages arrive as CTCP ACTION. *)
        if String.is_prefix text ~prefix:"\001ACTION " then
          let body = String.drop_prefix text 8 in
          let body = String.chop_suffix_if_exists body ~suffix:"\001" in
          sprintf "%s * %s %s" channel name body
        else sprintf "%s <%s> %s" channel name text
      in
      let line = Sanitize.(apply_filters line output_filters) ^ "\n" in
      Eio.Flow.copy_string line out
  | _ -> ()

let handle_line ws out line =
  let line = String.chop_suffix_if_exists line ~suffix:"\r" in
  match Irc.parse line with
  | Some { command = "PING"; params; _ } ->
      let token = Option.value (List.last params) ~default:host in
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
  let guest_id =
    Stdlib.String.get_uint16_be (Mirage_crypto_rng.generate 2) 0
  in
  Ws.send_text ws "CAP REQ :twitch.tv/tags twitch.tv/commands";
  Ws.send_text ws (sprintf "NICK justinfan%d" guest_id);
  Ws.send_text ws
    ("JOIN " ^ String.concat ~sep:"," (List.map channels ~f:(fun c -> "#" ^ c)));
  let rec loop () =
    match Ws.recv ws with
    | None -> ()
    | Some data ->
        List.iter (String.split data ~on:'\n') ~f:(handle_line ws out);
        loop ()
  in
  loop ()

let normalize_channel c =
  String.chop_prefix_if_exists (String.lowercase c) ~prefix:"#"

let ok_or_fail = function Ok x -> x | Error (`Msg m) -> failwith m

(* Reads [channels] until cancelled, reconnecting as needed. Must run inside
   Eio_main.run with the RNG already seeded. *)
let follow env channels =
  let channels = List.map channels ~f:normalize_channel in
  let authenticator = ok_or_fail (Ca_certs.authenticator ()) in
  let tls_config = ok_or_fail (Tls.Config.client ~authenticator ()) in
  let net = Eio.Stdenv.net env and out = Eio.Stdenv.stdout env in
  let rec forever () =
    (try run_session ~net ~tls_config ~out channels with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | e -> Eio.traceln "connection error: %s" (Exn.to_string e));
    Eio.traceln "disconnected, reconnecting in 2s";
    Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;
    forever ()
  in
  forever ()
