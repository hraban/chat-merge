(* Single entry point for all chat readers, busybox style: either
   "chat SUBCOMMAND ARGS..." or a symlink named after a subcommand's alias,
   e.g. "twitch-chat ARGS...". *)

open! Core
open Twitch_chat

let serve_flag =
  Command.Param.(
    flag "--serve" (optional int)
      ~doc:
        "PORT serve messages as a web page on 127.0.0.1:PORT instead of \
         printing" )

let twitch =
  Command.basic ~summary:"Read Twitch chat anonymously"
    Command.Param.(
      map
        (both serve_flag
           (anon (non_empty_sequence_as_list ("CHANNEL" %: string))) )
        ~f:(fun (serve, channels) () ->
          Eio_main.run
          @@ fun env ->
          Mirage_crypto_rng_unix.use_default () ;
          Output.with_sink env ~serve (fun stream ->
              Twitch.follow env ~stream channels ) ) )

let youtube =
  Command.basic ~summary:"Read YouTube live chat via the Data API"
    ~readme:(fun () ->
      "Targets are channel handles (@name), video IDs, or video URLs.\n\
       Needs an API key in $YOUTUBE_API_KEY." )
    Command.Param.(
      map
        (both serve_flag
           (anon (non_empty_sequence_as_list ("TARGET" %: string))) )
        ~f:(fun (serve, targets) () ->
          Eio_main.run
          @@ fun env ->
          Mirage_crypto_rng_unix.use_default () ;
          let key = Youtube.api_key env in
          Output.with_sink env ~serve (fun stream ->
              Youtube.follow_targets env ~key ~stream targets ) ) )

let stream =
  Command.basic ~summary:"Interleave Twitch and YouTube chat as it arrives"
    Command.Param.(
      map
        (both serve_flag
           (both
              (flag "--twitch" (optional string)
                 ~doc:"CHANNEL Twitch channel to read" )
              (flag "--youtube" (optional string)
                 ~doc:"TARGET YouTube @handle, video ID or URL" ) ) )
        ~f:(fun (serve, (twitch, youtube)) () ->
          Stream.run ~serve ?twitch ?youtube () ) )

(* Subcommand name, with the program name that runs it directly. *)
let commands =
  [ ("twitch", "twitch-chat", twitch)
  ; ("youtube", "youtube-chat", youtube)
  ; ("stream", "stream-chat", stream) ]

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
