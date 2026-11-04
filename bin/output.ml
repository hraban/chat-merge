(* Where the binary sends the messages the platform readers produce. *)

open! Core
open Twitch_chat

(* Runs [produce] with a fresh stream and prints each message on stdout as it
   arrives, until [produce] returns and everything still queued is printed. *)
let with_stdout_sink env produce =
  let stream = Eio.Stream.create 1024 in
  let out = Eio.Stdenv.stdout env in
  (* Protected so cancellation never leaves half a line on stdout. *)
  let print m =
    Eio.Cancel.protect (fun () ->
        Eio.Flow.copy_string (Chat_message.to_line m ^ "\n") out)
  in
  Eio.Fiber.first
    (fun () ->
      let rec loop () =
        print (Eio.Stream.take stream);
        loop ()
      in
      loop ())
    (fun () -> produce stream);
  (* Fiber.first cancelled the printer; flush what it hadn't got to yet. *)
  let rec drain () =
    Option.iter (Eio.Stream.take_nonblocking stream) ~f:(fun m ->
        print m;
        drain ())
  in
  drain ()
