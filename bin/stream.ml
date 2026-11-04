(* Twitch and YouTube chat interleaved on stdout as messages arrive. Either
   source may be left out. *)

open! Core
open Twitch_chat

let run ?twitch ?youtube () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  if Option.is_none twitch && Option.is_none youtube then (
    Eio.Flow.copy_string "stream: pass --twitch and/or --youtube\n"
      (Eio.Stdenv.stderr env);
    exit 2);
  (* Check for the key up front, not after Twitch output has started. *)
  let youtube = Option.map youtube ~f:(fun target -> (Youtube.api_key env, target)) in
  Eio.Fiber.both
    (fun () ->
      Option.iter twitch ~f:(fun channel -> Twitch.follow env [ channel ]))
    (fun () ->
      Option.iter youtube
        ~f:(fun (key, target) -> Youtube.follow_targets env ~key [ target ]))
