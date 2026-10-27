(lang dune 3.17)

(name twitch_chat)

(generate_opam_files true)

(package
 (name twitch_chat)
 (synopsis "Read Twitch chat anonymously over WebSocket")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.17))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  httpun
  (httpun-ws
   (>= 0.2.0))
  (httpun-ws-eio
   (>= 0.2.0))
  bigstringaf))
