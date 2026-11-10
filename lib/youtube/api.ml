(* The parts of the official YouTube Data API v3 needed to follow a live chat.
   https://developers.google.com/youtube/v3/live/docs/liveChatMessages *)

open! Core
open Option.Monad_infix

let ( let+ ) = ( >>| )

type error = {status: int; reason: string; message: string}

exception Api_error of error

(* $YOUTUBE_API_BASE_URL replaces this, so tests can talk to a fake server. *)
let base_url =
  match Sys.getenv "YOUTUBE_API_BASE_URL" with
  | None | Some "" ->
      "https://www.googleapis.com/youtube/v3/"
  | Some url ->
      url

let uri ~key endpoint params =
  Uri.with_query' (Uri.of_string (base_url ^ endpoint)) (("key", key) :: params)

(* The [Api_error] for a failed request's body. *)
let api_error ~status body =
  let error =
    match Api_json.error_payload_of_yojson (Yojson.Safe.from_string body) with
    | Api_json.Single r | Api_json.Many (r :: _) ->
        Some r.Api_json.error
    | Api_json.Many [] | (exception _) ->
        None
  in
  let reason =
    match error with
    | Some {Api_json.errors= e :: _; _} ->
        e.Api_json.reason
    | _ ->
        ""
  in
  let message =
    Option.value
      (error >>= fun (e : Api_json.error_body) -> e.message)
      ~default:body
  in
  Api_error {status; reason; message}

(* Decodes the response with [of_yojson]. A body that doesn't decode is an
   [Api_error] like any other. *)
let get of_yojson client ~key endpoint params =
  Eio.Switch.run
  @@ fun sw ->
  let resp, body =
    Cohttp_eio.Client.get ~sw client (uri ~key endpoint params)
  in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:(16 lsl 20) in
  match Cohttp.Code.code_of_status (Cohttp.Response.status resp) with
  | 200 -> (
    try of_yojson (Yojson.Safe.from_string body)
    with e ->
      raise (Api_error {status= 200; reason= "decode"; message= Exn.to_string e})
    )
  | status ->
      raise (api_error ~status body)

type video = {channel: string; live_chat_id: string option}

(* [None] if no video has that ID. [live_chat_id] is [None] unless the video
   is currently live with chat enabled. Costs 1 quota unit. *)
let video client ~key id =
  let r =
    get Api_json.videos_of_yojson client ~key "videos"
      [("part", "snippet,liveStreamingDetails"); ("id", id)]
  in
  let+ (v : Api_json.video) = List.hd r.items in
  { channel= Option.value v.snippet.channel_title ~default:id
  ; live_chat_id=
      ( v.live_streaming_details
      >>= fun (d : Api_json.live_streaming_details) -> d.active_live_chat_id )
  }

(* Channel ID for a handle such as "@name", or [None] if there is no such
   channel. Costs 1 quota unit. *)
let channel_id_of_handle client ~key handle =
  let r =
    get Api_json.channels_of_yojson client ~key "channels"
      [("part", "id"); ("forHandle", handle)]
  in
  List.hd r.items >>| fun (c : Api_json.channel) -> c.id

(* IDs of the channel's videos that are live right now. Search results can
   lag a stream going live by a few minutes. Costs 100 quota units. *)
let live_video_ids client ~key channel_id =
  let r =
    get Api_json.search_results_of_yojson client ~key "search"
      [ ("part", "id")
      ; ("channelId", channel_id)
      ; ("eventType", "live")
      ; ("type", "video") ]
  in
  List.filter_map r.items ~f:(fun (s : Api_json.search_result) ->
      s.id.video_id )

type message = {author: string; text: string}

type page =
  { messages: message list
  ; next_page_token: string option
        (** Resume from here if the connection drops. *)
  ; offline: bool  (** The stream has ended; no more messages will come. *) }

let page_of_json json =
  (* An error after the stream started arrives as an element of its own. *)
  match Api_json.error_response_of_yojson json with
  | r ->
      let status = Option.value r.Api_json.error.code ~default:500 in
      raise (api_error ~status (Yojson.Safe.to_string json))
  | exception _ -> (
    match Api_json.messages_of_yojson json with
    | r ->
        let message (m : Api_json.message) =
          match m.snippet.kind with
          | "textMessageEvent" ->
              Some
                { author=
                    Option.value
                      ( m.author_details
                      >>= fun (a : Api_json.author_details) -> a.display_name )
                      ~default:"?"
                ; text= m.snippet.display_message }
          | _ ->
              None
        in
        { messages= List.filter_map r.items ~f:message
        ; next_page_token= r.next_page_token
        ; offline= Option.is_some r.offline_at }
    | exception e ->
        raise
          (Api_error {status= 200; reason= "decode"; message= Exn.to_string e})
    )

(* Holds a liveChatMessages.stream connection open and calls [f] on each page
   of chat as the server pushes it. Without [page_token] the first page is the
   recent backlog; pass the last page's [next_page_token] to resume after a
   disconnect. Returns when the server closes the stream or after a page with
   [offline] set. Only plain text messages are returned. Costs 5 quota units
   per connection. *)
let stream_messages client ~key ~live_chat_id ?page_token ~f () =
  let params =
    [("liveChatId", live_chat_id); ("part", "snippet,authorDetails")]
    @ Option.value_map page_token ~default:[] ~f:(fun t -> [("pageToken", t)])
  in
  Eio.Switch.run
  @@ fun sw ->
  let resp, body =
    Cohttp_eio.Client.get ~sw client
      (uri ~key "liveChat/messages/stream" params)
  in
  match Cohttp.Code.code_of_status (Cohttp.Response.status resp) with
  | 200 ->
      let r = Eio.Buf_read.of_flow body ~max_size:(16 lsl 20) in
      (* Google APIs frame a server-streamed RPC over REST as a JSON array
         whose elements arrive one by one. *)
      Chat_merge.Json_stream.iter_array r ~f:(fun json ->
          let page = page_of_json json in
          f page ; not page.offline )
  | status ->
      let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:(16 lsl 20) in
      raise (api_error ~status body)
