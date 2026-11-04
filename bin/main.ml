(* Single entry point for all chat readers, busybox style: either
   "chat SUBCOMMAND ARGS..." or a symlink named after a subcommand's alias,
   e.g. "twitch-chat ARGS...". *)

open! Core

let stream =
  Command.basic ~summary:"Interleave Twitch and YouTube chat as it arrives"
    (let%map_open.Command serve =
       flag "--serve" (optional int)
         ~doc:"PORT serve messages as a web page instead of printing"
     and bind =
       flag "--bind"
         (optional_with_default "127.0.0.1" string)
         ~doc:"ADDR address for --serve to listen on (default 127.0.0.1)"
     and twitch =
       flag "--twitch" (optional string) ~doc:"CHANNEL Twitch channel to read"
     and youtube =
       flag "--youtube" (optional string)
         ~doc:"TARGET YouTube @handle, video ID or URL"
     in
     fun () -> Stream.run ~serve ~bind ?twitch ?youtube () )

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
