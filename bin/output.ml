(* Where the binary sends the messages the platform readers produce. *)

open! Core
open Chat_merge

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

let resolve_bind env bind =
  match Eio.Net.getaddrinfo_stream (Eio.Stdenv.net env) bind with
  | `Tcp (addr, _) :: _ ->
      addr
  | [] | `Unix _ :: _ ->
      Usage.error env "--bind: no address found for %S" bind
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed _), _) ->
      Usage.error env "--bind: cannot resolve %S" bind

(* Picks the sink from the --serve flag. [addr] is only forced when serving. *)
let with_sink env ~serve ~addr produce =
  match serve with
  | None ->
      with_stdout_sink env produce
  | Some port ->
      let addr = Lazy.force addr in
      Eio.traceln "serving on port %d of %a" port Eio.Net.Ipaddr.pp addr ;
      with_web_sink env ~addr ~port produce
