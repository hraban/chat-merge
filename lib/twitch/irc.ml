(* Parser for the IRCv3-style lines Twitch sends over the WebSocket, e.g.
   @color=#FF0000;display-name=Foo :foo!foo@foo.tmi.twitch.tv PRIVMSG #chan :hi *)

open! Core

type message =
  { tags: (string * string) list
  ; prefix: string option
  ; command: string
  ; params: string list  (** The trailing parameter, if any, comes last. *) }

let split_once c s =
  match String.lsplit2 s ~on:c with Some pair -> pair | None -> (s, "")

let parse_tags s = String.split s ~on:';' |> List.map ~f:(split_once '=')

(* Strips an optional leading [sigil]-introduced word, e.g. "@tags" or ":prefix". *)
let take_word_if sigil s =
  if String.is_prefix s ~prefix:(String.of_char sigil) then
    let word, rest = split_once ' ' s in
    (Some (String.drop_prefix word 1), rest)
  else (None, s)

let split_trailing s =
  match String.substr_index s ~pattern:" :" with
  | Some i ->
      (String.prefix s i, [String.drop_prefix s (i + 2)])
  | None ->
      (s, [])

let parse line =
  let tags, rest = take_word_if '@' line in
  let prefix, rest = take_word_if ':' rest in
  let middle, trailing = split_trailing rest in
  match
    String.split middle ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [] ->
      None
  | command :: params ->
      let tags = Option.value_map tags ~default:[] ~f:parse_tags in
      Some {tags; prefix; command; params= params @ trailing}

(* "nick!user@host" -> "nick" *)
let nick msg = Option.map msg.prefix ~f:(fun p -> fst (split_once '!' p))
