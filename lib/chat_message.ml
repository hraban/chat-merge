(* A chat message as the platform readers emit it, before formatting. *)

open! Core

type platform = Twitch | Youtube

type kind =
  | Say
  | Action  (** Twitch /me *)

type t = {
  platform : platform;
  channel : string;
      (** As shown to the user: "#chan" on Twitch, the channel title on
          YouTube. *)
  author : string;
  text : string;
  kind : kind;
}

(* The message as one printable line, without the trailing newline. Chat text
   is attacker-controlled, so the line goes through the output filters. *)
let to_line t =
  let line =
    match t.kind with
    | Say -> sprintf "%s <%s> %s" t.channel t.author t.text
    | Action -> sprintf "%s * %s %s" t.channel t.author t.text
  in
  Sanitize.(apply_filters line output_filters)
