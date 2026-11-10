(* Incremental reading of JSON from an Eio flow: values are handed over as
   soon as they have arrived, without waiting for the rest of the input. *)

open! Core

(* A lexbuf that reads from [r], handing the lexer whatever bytes are already
   buffered rather than waiting to fill its whole buffer, so values are
   decoded as soon as they have arrived. [End_of_file] from [r] propagates. *)
let lexbuf_of_buf_read r =
  Lexing.from_function (fun bytes n ->
      Eio.Buf_read.ensure r 1 ;
      let available = Eio.Buf_read.peek r in
      let len = Int.min n (Cstruct.length available) in
      Cstruct.blit_to_bytes available 0 bytes 0 len ;
      Eio.Buf_read.consume r len ;
      len )

(* Calls [f] on each element of a JSON array as soon as it has arrived, until
   the array ends or [f] returns [false]. Input that ends mid-array raises
   [End_of_file]. *)
let iter_array r ~f =
  let exception Stop in
  let lexbuf = lexbuf_of_buf_read r in
  let v = Yojson.init_lexer () in
  try
    Yojson.Safe.read_sequence
      (fun () v lexbuf ->
        if not (f (Yojson.Safe.read_json v lexbuf)) then raise Stop )
      () v lexbuf
  with Stop -> ()

let%expect_test "iter_array" =
  let show s =
    let r = Eio.Buf_read.of_string s in
    match
      iter_array r ~f:(fun o ->
          let o = Yojson.Safe.to_string o in
          print_endline o ;
          not (String.equal o {|{"stop":1}|}) )
    with
    | () ->
        print_endline "-- end"
    | exception e ->
        print_endline ("-- " ^ Exn.to_string e)
  in
  show "[]" ;
  show {|[{"a":"}{\"]"}
,{"b":[{"c":{}}]}
]|} ;
  show {|[{"a":1},{"stop":1},{"never":1}]|} ;
  show {|[{"a":1}
,{"b"|} ;
  [%expect
    {|
    -- end
    {"a":"}{\"]"}
    {"b":[{"c":{}}]}
    -- end
    {"a":1}
    {"stop":1}
    -- end
    {"a":1}
    -- End_of_file
    |}]
