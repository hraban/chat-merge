(lang dune 3.17)

(name chat_merge)

(cram enable)

; (generate_opam_files true)

(package
 (name chat_merge)
 (synopsis "Read Twitch chat anonymously over WebSocket")
 (depends
  ocaml
  dune
  base64
  ca-certs
  cohttp
  cohttp-eio
  core
  core_unix
  domain-name
  eio
  eio_main
  http
  mirage-crypto-rng
  ppx_expect
  ppx_let
  tls
  tls-eio
  uri
  yojson
  atdml))
