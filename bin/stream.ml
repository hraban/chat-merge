(* Twitch and YouTube chat interleaved on stdout as messages arrive. Either
   source may be left out. *)

let run ?twitch ?youtube () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  if twitch = None && youtube = None then (
    Eio.Flow.copy_string "stream: pass --twitch and/or --youtube\n"
      (Eio.Stdenv.stderr env);
    exit 2);
  (* Check for the key up front, not after Twitch output has started. *)
  let youtube = Option.map (fun target -> (Youtube.api_key env, target)) youtube in
  Eio.Fiber.both
    (fun () -> Option.iter (fun channel -> Twitch.follow env [ channel ]) twitch)
    (fun () ->
      Option.iter
        (fun (key, target) -> Youtube.follow_targets env ~key [ target ])
        youtube)
