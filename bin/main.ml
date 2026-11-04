(* Single entry point for all chat readers, busybox style: either
   "chat SUBCOMMAND ARGS..." or a symlink named after a subcommand's alias,
   e.g. "twitch-chat ARGS...". *)

open! Core

let serve_flag =
  Command.Param.(
    flag "--serve" (optional int)
      ~doc:
        "PORT serve messages as a web page on 127.0.0.1:PORT instead of \
         printing" )

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
let commands = [("stream", "stream-chat", stream)]

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
