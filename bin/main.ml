(* Single entry point for all chat readers, busybox style: either
   "chat SUBCOMMAND ARGS..." or a symlink named after a subcommand's alias,
   e.g. "twitch-chat ARGS...". *)

open! Core

let stream =
  Command.basic ~summary:"Interleave Twitch and YouTube chat as it arrives"
    (let%map_open.Command serve =
       flag "--serve" no_arg
         ~doc:" serve messages as a web page instead of printing"
     and port =
       flag "--port"
         (optional_with_default 8080 int)
         ~doc:
           "PORT port for the web page and Twitch webhooks to listen on \
            (default 8080)"
     and bind =
       flag "--bind"
         (optional_with_default "127.0.0.1" string)
         ~doc:
           "ADDR address for the web page and Twitch webhooks to listen on \
            (default 127.0.0.1)"
     and twitch =
       flag "--twitch" (optional string) ~doc:"CHANNEL Twitch channel to read"
     and public_url =
       flag "--public-url" (optional string)
         ~doc:
           "URL public HTTPS base URL (port 443) that reaches --port; Twitch \
            posts chat to URL/twitch/eventsub (required with --twitch)"
     and twitch_as =
       flag "--twitch-as" (optional string)
         ~doc:"LOGIN Twitch user to read chat as (default: the broadcaster)"
     and youtube =
       flag "--youtube" (optional string)
         ~doc:"TARGET YouTube @handle, video ID or URL"
     in
     fun () ->
       Stream.run ~serve ~port ~bind ?twitch ?public_url ?twitch_as ?youtube ()
    )

let twitch_auth_url =
  Command.basic
    ~summary:"Print the URL that grants the app access to a user's Twitch chat"
    ~readme:(fun () ->
      "Reading chat with an app token needs the broadcaster to grant the app \
       channel:bot, and the user chat is read as (--twitch-as, by default the \
       broadcaster) to grant user:read:chat and user:bot. Each of them opens \
       this URL and clicks Authorize once. The redirect URI must be registered \
       for the app in the Twitch developer console; what it receives is not \
       needed." )
    (let%map_open.Command redirect_uri =
       flag "--redirect-uri"
         (optional_with_default "http://localhost" string)
         ~doc:"URI registered redirect URI (default http://localhost)"
     in
     fun () ->
       match Sys.getenv "TWITCH_CLIENT_ID" with
       | None | Some "" ->
           prerr_endline "twitch-auth-url: set TWITCH_CLIENT_ID" ;
           exit 2
       | Some client_id ->
           print_endline (Twitch.Helix.authorize_url ~client_id ~redirect_uri)
    )

(* Subcommand name, with the program name that runs it directly. *)
let commands =
  [ ("stream", "stream-chat", stream)
  ; ("twitch-auth-url", "twitch-auth-url", twitch_auth_url) ]

let () =
  (* Not Sys.executable_name: that resolves the symlink we dispatch on. *)
  let invoked_as = Filename.basename (Sys.get_argv ()).(0) in
  match
    List.find commands ~f:(fun (_, alias, _) -> String.equal alias invoked_as)
  with
  | Some (_, _, command) ->
      Command_unix.run command
  | None ->
      Command_unix.run
        (Command.group ~summary:"Read live chat from streaming platforms"
           (List.map commands ~f:(fun (name, _, command) -> (name, command))) )
