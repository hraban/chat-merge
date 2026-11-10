(* The fields of YouTube Data API v3 responses that Api reads. Unknown fields
   are ignored. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type error_item = {reason: string [@default ""]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type error_body =
  { code: int option [@yojson.option]
  ; errors: error_item list [@default []]
  ; message: string option [@yojson.option] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type error_response = {error: error_body}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(* The body of a failed request: the usual error object, or (from streaming
   endpoints) that object wrapped in an array. *)
type error_payload = Single of error_response | Many of error_response list

let error_payload_of_yojson = function
  | `List _ as json ->
      Many (list_of_yojson error_response_of_yojson json)
  | json ->
      Single (error_response_of_yojson json)

type video_snippet =
  {channel_title: string option [@key "channelTitle"] [@yojson.option]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type live_streaming_details =
  {active_live_chat_id: string option [@key "activeLiveChatId"] [@yojson.option]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type video =
  { snippet: video_snippet
  ; live_streaming_details: live_streaming_details option
        [@key "liveStreamingDetails"] [@yojson.option] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type videos = {items: video list [@default []]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type channel = {id: string}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type channels = {items: channel list [@default []]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type search_id = {video_id: string option [@key "videoId"] [@yojson.option]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type search_result = {id: search_id}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type search_results = {items: search_result list [@default []]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type message_snippet =
  { kind: string [@key "type"] [@default ""]
  ; display_message: string [@key "displayMessage"] [@default ""] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type author_details =
  {display_name: string option [@key "displayName"] [@yojson.option]}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type message =
  { snippet: message_snippet
  ; author_details: author_details option
        [@key "authorDetails"] [@yojson.option] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

type messages =
  { items: message list [@default []]
  ; next_page_token: string option [@key "nextPageToken"] [@yojson.option]
  ; offline_at: string option [@key "offlineAt"] [@yojson.option] }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]
