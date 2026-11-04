(* A chat message as the platform readers emit it, before formatting. *)

open! Core

type platform = Twitch | Youtube

type kind = Say | Action  (** Twitch /me *)

(* A Twitch emote occurrence: [first..last] (inclusive) are Unicode code point
   indices into [text]. *)
type emote = {id: string; first: int; last: int}

type t =
  { platform: platform
  ; channel: string
        (** As shown to the user: "#chan" on Twitch, the channel title on
          YouTube. *)
  ; author: string
  ; text: string
  ; kind: kind
  ; emotes: emote list }

type segment = Text of string | Emote of {id: string; name: string}

(* The message as one printable line, without the trailing newline. Chat text
   is attacker-controlled, so the line goes through the output filters. *)
let to_line t =
  let line =
    match t.kind with
    | Say ->
        sprintf "<%s> %s" t.author t.text
    | Action ->
        sprintf "* %s %s" t.author t.text
  in
  Sanitize.(apply_filters line output_filters)

(* The "<author> " / "* author " part that precedes the text, sanitized. *)
let head t =
  let h =
    match t.kind with
    | Say ->
        sprintf "<%s> " t.author
    | Action ->
        sprintf "* %s " t.author
  in
  Sanitize.(apply_filters h output_filters)

(* Emote ids end up in a URL, and come from the network. *)
let valid_emote_id id =
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

(* The text split into plain runs and emotes, all sanitized. Emotes that are
   malformed, out of range or overlapping are left as plain text. *)
let segments t =
  let offs = codepoint_offsets t.text in
  let ncp = Array.length offs - 1 in
  let sub a b = String.sub t.text ~pos:offs.(a) ~len:(offs.(b) - offs.(a)) in
  let clean s = Sanitize.(apply_filters s output_filters) in
  let emotes =
    List.filter t.emotes ~f:(fun e ->
        valid_emote_id e.id && 0 <= e.first && e.first <= e.last && e.last < ncp )
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
    let t =
      {platform= Twitch; channel= "#c"; author= "a"; text; kind= Say; emotes}
    in
    List.iter (segments t) ~f:(function
      | Text s ->
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

let%expect_test "to_line sanitizes chat text" =
  print_endline
    (to_line
       { platform= Youtube
       ; channel= "Some Channel"
       ; author= "mallory"
       ; text= "\027[2Jgotcha\xe2\x80\xae"
       ; kind= Say
       ; emotes= [] } ) ;
  [%expect {| <mallory> ⟨U+001B⟩[2Jgotcha⟨U+202E⟩ |}]
