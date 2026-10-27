(* Parser for the IRCv3-style lines Twitch sends over the WebSocket, e.g.
   @color=#FF0000;display-name=Foo :foo!foo@foo.tmi.twitch.tv PRIVMSG #chan :hi *)

type message = {
  tags : (string * string) list;
  prefix : string option;
  command : string;
  params : string list;  (** The trailing parameter, if any, comes last. *)
}

let split_once c s =
  match String.index_opt s c with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> (s, "")

let parse_tags s =
  String.split_on_char ';' s |> List.map (split_once '=')

(* Strips an optional leading [sigil]-introduced word, e.g. "@tags" or ":prefix". *)
let take_word_if sigil s =
  if String.length s > 0 && s.[0] = sigil then
    let word, rest = split_once ' ' s in
    (Some (String.sub word 1 (String.length word - 1)), rest)
  else (None, s)

let split_trailing s =
  let n = String.length s in
  let rec find i =
    if i + 1 >= n then None
    else if s.[i] = ' ' && s.[i + 1] = ':' then Some i
    else find (i + 1)
  in
  match find 0 with
  | Some i -> (String.sub s 0 i, [ String.sub s (i + 2) (n - i - 2) ])
  | None -> (s, [])

let parse line =
  let tags, rest = take_word_if '@' line in
  let prefix, rest = take_word_if ':' rest in
  let middle, trailing = split_trailing rest in
  match String.split_on_char ' ' middle |> List.filter (( <> ) "") with
  | [] -> None
  | command :: params ->
      let tags = Option.fold ~none:[] ~some:parse_tags tags in
      Some { tags; prefix; command; params = params @ trailing }

(* "nick!user@host" -> "nick" *)
let nick msg = Option.map (fun p -> fst (split_once '!' p)) msg.prefix
