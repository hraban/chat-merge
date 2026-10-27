(* Anonymous Twitch chat reader. Connects to Twitch's WebSocket chat endpoint
   as a "justinfan" guest and prints every chat message to stdout. *)

open Twitch_chat

(* Plain ws:// for now; Twitch also serves wss:// on 443. *)
let host = "irc-ws.chat.twitch.tv"
let port = 80

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
          Printf.sprintf "%s * %s %s\n" channel name body
        else Printf.sprintf "%s <%s> %s\n" channel name text
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
let run_session ~net ~out channels =
  Eio.Net.with_tcp_connect net ~host ~service:(string_of_int port)
  @@ fun sock ->
  Eio.Switch.run @@ fun sw ->
  let ws = Ws.connect ~sw ~host ~port sock in
  Ws.send_text ws "CAP REQ :twitch.tv/tags twitch.tv/commands";
  Ws.send_text ws (Printf.sprintf "NICK justinfan%d" (Random.int 100_000));
  Ws.send_text ws
    ("JOIN " ^ String.concat "," (List.map (fun c -> "#" ^ c) channels));
  let rec loop () =
    match Ws.recv ws with
    | `Closed reason -> Eio.traceln "connection closed: %s" reason
    | `Message data ->
        List.iter (handle_line ws out) (String.split_on_char '\n' data);
        loop ()
  in
  loop ();
  Ws.close ws

let normalize_channel c =
  let c = String.lowercase_ascii c in
  if String.starts_with ~prefix:"#" c then
    String.sub c 1 (String.length c - 1)
  else c

let () =
  Eio_main.run @@ fun env ->
  (* Seeds the WebSocket nonce and frame masks, and the guest nick. *)
  Random.self_init ();
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
      Eio.Flow.copy_string "usage: twitch-chat CHANNEL...\n"
        (Eio.Stdenv.stderr env);
      exit 2
  | channels ->
      let channels = List.map normalize_channel channels in
      let net = Eio.Stdenv.net env and out = Eio.Stdenv.stdout env in
      let rec forever () =
        (try run_session ~net ~out channels with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e -> Eio.traceln "connection error: %s" (Printexc.to_string e));
        Eio.traceln "disconnected, reconnecting in 2s";
        Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;
        forever ()
      in
      forever ()
