(* Where the binary sends the messages the platform readers produce. *)

open! Core
open Chat_merge

(* Runs [produce] and prints each message on [stream] on stdout as it arrives,
   until [produce] returns and everything still queued is printed. *)
let with_stdout_sink env ~stream produce =
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
    produce ;
  (* Fiber.first cancelled the printer; flush what it hadn't got to yet. *)
  let rec drain () =
    Option.iter (Eio.Stream.take_nonblocking stream) ~f:(fun m ->
        print m ; drain () )
  in
  drain ()

(* Listens on [addr]:[port] until [sw] finishes. *)
let listen env ~sw ~addr ~port =
  match
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~backlog:32
      (`Tcp (addr, port))
  with
  | socket ->
      Eio.traceln "listening on port %d of %a" port Eio.Net.Ipaddr.pp addr ;
      socket
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (EADDRINUSE, _, _)), _) ->
      Usage.error env "--port: %s port %d already in use"
        (Format.asprintf "%a" Eio.Net.Ipaddr.pp addr)
        port

let resolve_bind env bind =
  match Eio.Net.getaddrinfo_stream (Eio.Stdenv.net env) bind with
  | `Tcp (addr, _) :: _ ->
      addr
  | [] | `Unix _ :: _ ->
      Usage.error env "--bind: no address found for %S" bind
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed _), _) ->
      Usage.error env "--bind: cannot resolve %S" bind

(* Runs [produce], sending the messages on [stream] to the chat page [web] if
   given, else to stdout. *)
let with_sink env ~web ~stream produce =
  match web with
  | None ->
      with_stdout_sink env ~stream produce
  | Some web ->
      Eio.Fiber.first (fun () -> Web_server.pump web stream) produce
