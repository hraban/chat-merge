(lang dune 3.17)

(name chat_merge)

(cram enable)

; (generate_opam_files true)

(package
 (name chat_merge)
 (synopsis "Merge Twitch and YouTube live chat into one stream")
 (depends
  ocaml
  dune
  base64
  ca-certs
  cohttp
  cohttp-eio
  core
  core_unix
  digestif
  domain-name
  eio
  eio_main
  eqaf
  http
  mirage-crypto-rng
  ppx_blob
  ppx_expect
  ppx_jane
  ppx_let
  tls
  tls-eio
  uri
  yojson
  ppx_yojson_conv))
