(lang dune 3.17)

(name chat_merge)

(cram enable)

; (generate_opam_files true)

(package
 (name chat_merge)
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
  (tls-eio
   (>= 1.0))
  (ca-certs
   (>= 1.0))
  (mirage-crypto-rng
   (>= 1.1))
  domain-name
  base64
  (cohttp-eio
   (>= 6.0.0))
  uri
  (yojson
   (>= 2.0))
  (core
   (>= v0.16))
  (core_unix
   (>= v0.16))))
