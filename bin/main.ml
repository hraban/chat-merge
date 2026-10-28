(* Single entry point for all chat readers, busybox style: either
   "chat SUBCOMMAND ARGS..." or a symlink named after a subcommand's alias,
   e.g. "twitch-chat ARGS...". *)

module Command = Core.Command

let twitch =
  Command.basic ~summary:"Read Twitch chat anonymously"
    Command.Param.(
      map
        (anon (non_empty_sequence_as_list ("CHANNEL" %: string)))
        ~f:(fun channels () -> Twitch.run channels))

let youtube =
  Command.basic ~summary:"Read YouTube live chat via the Data API"
    ~readme:(fun () ->
      "Targets are channel handles (@name), video IDs, or video URLs.\n\
       Needs an API key in $YOUTUBE_API_KEY.")
    Command.Param.(
      map
        (anon (non_empty_sequence_as_list ("TARGET" %: string)))
        ~f:(fun targets () -> Youtube.run targets))

(* Subcommand name, with the program name that runs it directly. *)
let commands = [ ("twitch", "twitch-chat", twitch); ("youtube", "youtube-chat", youtube) ]

let () =
  (* Not Sys.executable_name: that resolves the symlink we dispatch on. *)
  let invoked_as = Filename.basename Sys.argv.(0) in
  match List.find_opt (fun (_, alias, _) -> alias = invoked_as) commands with
  | Some (_, _, command) -> Command_unix.run command
  | None ->
      Command_unix.run
        (Command.group ~summary:"Read live chat from streaming platforms"
           (List.map (fun (name, _, command) -> (name, command)) commands))
