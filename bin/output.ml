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
        let channel =
          match m.Chat_message.platform with Twitch -> "T" | Youtube -> "Y"
        in
        Eio.Buf_write.with_flow out
        @@ fun w ->
        Eio.Buf_write.printf w "%s %s\n" channel (Chat_message.to_line m) )
  in
  Eio.Fiber.first
    (fun () ->
      let rec loop () =
        print (Eio.Stream.take stream) ;
        loop ()
      in
      loop () )
    (fun () -> produce stream) ;
  (* Fiber.first cancelled the printer; flush what it hadn't got to yet. *)
  let rec drain () =
    Option.iter (Eio.Stream.take_nonblocking stream) ~f:(fun m ->
        print m ; drain () )
  in
  drain ()

(* Like [with_stdout_sink], but serves the messages to browsers on
   [addr]:[port] instead; runs until [produce] returns. *)
let with_web_sink env ~addr ~port produce =
  let stream = Eio.Stream.create 1024 in
  Eio.Fiber.first
    (fun () -> Web_server.run env ~addr ~port ~stream)
    (fun () -> produce stream)

(* Picks the sink from the --serve flag. *)
let with_sink env ~serve produce =
  match serve with
  | None ->
      with_stdout_sink env produce
  | Some port ->
      Eio.traceln "serving on http://127.0.0.1:%d/" port ;
      with_web_sink env ~addr:Eio.Net.Ipaddr.V4.loopback ~port produce
