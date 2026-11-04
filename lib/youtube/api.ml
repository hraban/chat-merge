(* The parts of the official YouTube Data API v3 needed to follow a live chat.
   https://developers.google.com/youtube/v3/live/docs/liveChatMessages *)

open! Core

type error = {status: int; reason: string; message: string}

exception Api_error of error

let base_url = "https://www.googleapis.com/youtube/v3/"

(* Decodes the response with [of_json]. A body that doesn't decode is an
   [Api_error] like any other. *)
let get of_json client ~key endpoint params =
  let uri =
    Uri.with_query'
      (Uri.of_string (base_url ^ endpoint))
      (("key", key) :: params)
  in
  Eio.Switch.run
  @@ fun sw ->
  let resp, body = Cohttp_eio.Client.get ~sw client uri in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:(16 lsl 20) in
  match Cohttp.Code.code_of_status (Cohttp.Response.status resp) with
  | 200 -> (
    try of_json body
    with e ->
      raise (Api_error {status= 200; reason= "decode"; message= Exn.to_string e})
    )
  | status ->
      let error =
        Option.try_with (fun () ->
            (Api_json.error_response_of_json body).Api_json.error )
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
          (Option.bind error ~f:(fun (e : Api_json.error_body) -> e.message))
          ~default:body
      in
      raise (Api_error {status; reason; message})

type video = {channel: string; live_chat_id: string option}

(* [None] if no video has that ID. [live_chat_id] is [None] unless the video
   is currently live with chat enabled. Costs 1 quota unit. *)
let video client ~key id =
  let r =
    get Api_json.videos_of_json client ~key "videos"
      [("part", "snippet,liveStreamingDetails"); ("id", id)]
  in
  List.hd r.items
  |> Option.map ~f:(fun (v : Api_json.video) ->
      { channel= Option.value v.snippet.channel_title ~default:id
      ; live_chat_id=
          Option.bind v.live_streaming_details
            ~f:(fun (d : Api_json.live_streaming_details) ->
              d.active_live_chat_id ) } )

(* Channel ID for a handle such as "@name", or [None] if there is no such
   channel. Costs 1 quota unit. *)
let channel_id_of_handle client ~key handle =
  let r =
    get Api_json.channels_of_json client ~key "channels"
      [("part", "id"); ("forHandle", handle)]
  in
  List.hd r.items |> Option.map ~f:(fun (c : Api_json.channel) -> c.id)

(* IDs of the channel's videos that are live right now. Search results can
   lag a stream going live by a few minutes. Costs 100 quota units. *)
let live_video_ids client ~key channel_id =
  let r =
    get Api_json.search_results_of_json client ~key "search"
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
  ; polling_interval: float  (** Seconds the server asks us to wait. *)
  ; offline: bool  (** The stream has ended; no more messages will come. *) }

(* One page of chat. Without [page_token] this is the recent backlog; pass the
   previous page's [next_page_token] to get only what arrived since. Only
   plain text messages are returned. Costs 5 quota units. *)
let messages client ~key ~live_chat_id ?page_token () =
  let params =
    [ ("liveChatId", live_chat_id)
    ; ("part", "snippet,authorDetails")
    ; ("maxResults", "2000") ]
    @ Option.value_map page_token ~default:[] ~f:(fun t -> [("pageToken", t)])
  in
  let r =
    get Api_json.messages_of_json client ~key "liveChat/messages" params
  in
  let message (m : Api_json.message) =
    match m.snippet.kind with
    | "textMessageEvent" ->
        Some
          { author=
              Option.value
                (Option.bind m.author_details
                   ~f:(fun (a : Api_json.author_details) -> a.display_name ) )
                ~default:"?"
          ; text= m.snippet.display_message }
    | _ ->
        None
  in
  { messages= List.filter_map r.items ~f:message
  ; next_page_token= r.next_page_token
  ; polling_interval= Float.of_int r.polling_interval_millis /. 1000.
  ; offline= Option.is_some r.offline_at }
