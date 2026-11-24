(* Reporting bad command-line usage of the stream subcommand. *)

open! Core

let error env fmt =
  ksprintf
    (fun msg ->
      Eio.Flow.copy_string ("stream: " ^ msg ^ "\n") (Eio.Stdenv.stderr env) ;
      exit 2 )
    fmt
