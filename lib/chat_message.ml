(* A chat message as the platform readers emit it, before formatting. *)

open! Core

type platform = Twitch | Youtube

type kind = Say | Action  (** Twitch /me *)

(* A run of message text, or an emote shown in its place. Text is already
   sanitized; emote ids are safe to put in a URL. *)
type segment = Text of string | Emote of {id: string; name: string}

type t =
  { platform: platform
  ; channel: string
        (** As shown to the user: "#chan" on Twitch, the channel title on
          YouTube. *)
  ; author: string
  ; text: string
  ; kind: kind
  ; segments: segment list }

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
  Sanitize.clean line

(* The "<author> " / "* author " part that precedes the text, sanitized. *)
let head t =
  let h =
    match t.kind with
    | Say ->
        sprintf "<%s> " t.author
    | Action ->
        sprintf "* %s " t.author
  in
  Sanitize.clean h

(* A message without emotes: all of [text], sanitized. *)
let plain_segments text =
  if String.is_empty text then []
  else [Text (Sanitize.clean text)]

let%expect_test "to_line sanitizes chat text" =
  print_endline
    (to_line
       { platform= Youtube
       ; channel= "Some Channel"
       ; author= "mallory"
       ; text= "\027[2Jgotcha\xe2\x80\xae"
       ; kind= Say
       ; segments= [] } ) ;
  [%expect {| <mallory> ⟨U+001B⟩[2Jgotcha⟨U+202E⟩ |}]
