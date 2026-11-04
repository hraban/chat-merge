(* Twitch emotes: where they occur in a message, and splitting the message
   text around them. *)

open! Core

(* One occurrence: [first..last] (inclusive) are Unicode code point indices
   into the message text. *)
type t = {id: string; first: int; last: int}

(* "25:0-4,12-16/1902:6-10" (the IRC [emotes] tag) -> occurrences. [shift] is
   subtracted from every index (the text may have lost a prefix). Malformed
   entries are dropped. *)
let parse_tag ~shift tag : t list =
  String.split tag ~on:'/'
  |> List.concat_map ~f:(fun entry ->
      match String.lsplit2 entry ~on:':' with
      | None ->
          []
      | Some (id, ranges) ->
          String.split ranges ~on:','
          |> List.filter_map ~f:(fun r ->
              match String.lsplit2 r ~on:'-' with
              | Some (a, b) -> (
                match (Int.of_string_opt a, Int.of_string_opt b) with
                | Some a, Some b ->
                    Some {id; first= a - shift; last= b - shift}
                | _ ->
                    None )
              | None ->
                  None ) )

let%expect_test "parse_tag" =
  let show ~shift tag =
    parse_tag ~shift tag
    |> List.iter ~f:(fun {id; first; last} -> printf "%s %d-%d\n" id first last)
  in
  show ~shift:0 "25:0-4,12-16/1902:6-10" ;
  [%expect {|
    25 0-4
    25 12-16
    1902 6-10
    |}] ;
  show ~shift:8 "25:8-12/bogus/x:a-b" ;
  [%expect {| 25 0-4 |}] ;
  show ~shift:0 "" ;
  [%expect {||}]

(* Emote ids end up in a URL, and come from the network. *)
let valid_id id =
  (not (String.is_empty id))
  && String.for_all id ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')

(* Byte offset of every code point of [s], plus the length of [s] at the end. *)
let codepoint_offsets s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev (n :: acc)
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      go (i + Stdlib.Uchar.utf_decode_length d) (i :: acc)
  in
  Array.of_list (go 0 [])

(* [text] split into plain runs and emotes, all sanitized. Emotes that are
   malformed, out of range or overlapping are left as plain text. *)
let segments ~text emotes : Chat_message.segment list =
  let open Chat_message in
  let offs = codepoint_offsets text in
  let ncp = Array.length offs - 1 in
  let sub a b = String.sub text ~pos:offs.(a) ~len:(offs.(b) - offs.(a)) in
  let clean s = Sanitize.(apply_filters s output_filters) in
  let emotes =
    List.filter emotes ~f:(fun e ->
        valid_id e.id && 0 <= e.first && e.first <= e.last && e.last < ncp )
    |> List.sort ~compare:(fun a b -> Int.compare a.first b.first)
  in
  let text_seg a b = if a < b then [Text (clean (sub a b))] else [] in
  let rec go pos = function
    | [] ->
        text_seg pos ncp
    | e :: rest when e.first >= pos ->
        text_seg pos e.first
        @ Emote {id= e.id; name= clean (sub e.first (e.last + 1))}
          :: go (e.last + 1) rest
    | _ :: rest ->
        go pos rest
  in
  go 0 emotes

let%expect_test "segments" =
  let show text emotes =
    List.iter (segments ~text emotes) ~f:(function
      | Chat_message.Text s ->
          printf "text %S\n" s
      | Emote {id; name} ->
          printf "emote %s %S\n" id name )
  in
  show "hi Kappa é LUL!"
    [{id= "25"; first= 3; last= 7}; {id= "425618"; first= 11; last= 13}] ;
  [%expect
    {|
    text "hi "
    emote 25 "Kappa"
    text " \195\169 "
    emote 425618 "LUL"
    text "!"
    |}] ;
  (* Out of range, overlapping and non-alphanumeric ids are ignored. *)
  show "abc"
    [ {id= "1"; first= 2; last= 9}
    ; {id= "../x"; first= 0; last= 0}
    ; {id= "2"; first= 0; last= 1}
    ; {id= "3"; first= 1; last= 2} ] ;
  [%expect {|
    emote 2 "ab"
    text "c"
    |}]
