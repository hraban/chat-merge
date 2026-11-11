(* The fields of Twitch Helix and EventSub JSON that Helix and Eventsub read.
   Unknown fields are ignored. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type token = {access_token: string}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* Helix error bodies: {"error": "Forbidden", "status": 403, "message": "..."}. *)
type error = {message: string [@default ""]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type user = {id: string; login: string}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type users = {data: user list [@default []]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type transport =
  {method_: string [@key "method"]; callback: string option [@yojson.option]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type subscription =
  { id: string
  ; status: string [@default ""]
  ; type_: string [@key "type"]
  ; transport: transport }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type pagination = {cursor: string option [@yojson.option]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type subscriptions =
  { data: subscription list [@default []]
  ; pagination: pagination option [@yojson.option] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* EventSub webhook bodies. *)

type emote = {id: string} [@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* Twitch sends every per-type field, set to null for the other types. *)
type fragment =
  { type_: string [@key "type"]
  ; text: string [@default ""]
  ; emote: emote option [@default None] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type chat_text = {text: string; fragments: fragment list [@default []]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* A channel.chat.message event. *)
type chat_event =
  { broadcaster_user_login: string
  ; chatter_user_login: string [@default ""]
  ; chatter_user_name: string [@default ""]
  ; color: string [@default ""]
  ; message: chat_text }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type webhook_subscription =
  {status: string [@default ""]; type_: string [@key "type"] [@default ""]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type verification = {challenge: string}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type json = Yojson.Safe.t

let json_of_yojson (j : json) = j

type notification =
  {subscription: webhook_subscription; event: json [@default `Null]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]
