(* YouTube live chat reader. Follows one or more live streams through the
   official YouTube Data API v3 and passes on every chat message.
   Needs an API key in $YOUTUBE_API_KEY. *)

open! Core

(* Each stream connection costs 5 units of the default 10,000/day quota, so
   never open them faster than this, whether reconnecting after the server
   closed the stream or retrying after an error. *)
let min_reconnect_interval = 5.0

(* Accepts a channel handle (@name or youtube.com/@name), a bare video ID, or
   a youtube.com/watch?v=, youtube.com/live/ or youtu.be/ URL. *)
let target_of_arg arg =
  let uri = Uri.of_string arg in
  let segments =
    List.filter
      (String.split (Uri.path uri) ~on:'/')
      ~f:(Fn.non String.is_empty)
  in
  match (Uri.host uri, Uri.get_query_param uri "v", segments) with
  | None, _, _ when String.is_prefix arg ~prefix:"@" ->
      `Handle arg
  | None, _, _ ->
      `Video arg
  | Some _, Some v, _ ->
      `Video v
  | Some _, None, first :: _ when String.is_prefix first ~prefix:"@" ->
      `Handle first
  | Some _, None, segments -> (
    match List.last segments with
    | Some last ->
        `Video last
    | None ->
        `Video arg )

(* The live video IDs an argument refers to; a channel may have several. *)
let video_ids ~client ~key arg =
  match target_of_arg arg with
  | `Video id ->
      [id]
  | `Handle handle -> (
    match Api.channel_id_of_handle client ~key handle with
    | None ->
        Eio.traceln "%s: no such channel" handle ;
        []
    | Some channel_id -> (
      match Api.live_video_ids client ~key channel_id with
      | [] ->
          Eio.traceln "%s: not live right now" handle ;
          []
      | ids ->
          ids ) )

let add_message stream channel (msg : Api.message) =
  Eio.Stream.add stream
    { Chat_merge.Chat_message.platform= Youtube
    ; channel
    ; author= msg.author
    ; text= msg.text
    ; kind= Say
    ; color= None
    ; segments= Chat_merge.Chat_message.plain_segments msg.text }

(* Follows one stream's chat until it ends, reconnecting with the last page
   token whenever the connection closes. Network and server (5xx) errors are
   retried; client (4xx) errors give up. *)
let follow ~clock ~client ~key ~stream video_id =
  let listen ~channel ~live_chat_id =
    let page_token = ref None in
    let offline = ref false in
    let on_page (page : Api.page) =
      List.iter page.messages ~f:(add_message stream channel) ;
      Option.iter page.next_page_token ~f:(fun t -> page_token := Some t) ;
      if page.offline then offline := true
    in
    let rec connect () =
      let started = Eio.Time.now clock in
      let wait_and_reconnect () =
        Eio.Time.sleep_until clock (started +. min_reconnect_interval) ;
        connect ()
      in
      match
        Api.stream_messages client ~key ~live_chat_id ?page_token:!page_token
          ~f:on_page ()
      with
      | () when !offline ->
          Eio.traceln "%s: stream ended" channel
      | () ->
          wait_and_reconnect ()
      | exception (Api.Api_error {status; _} as e) when status < 500 ->
          raise e
      | exception (Eio.Cancel.Cancelled _ as e) ->
          raise e
      | exception e ->
          Eio.traceln "%s: %s, reconnecting" channel (Exn.to_string e) ;
          wait_and_reconnect ()
    in
    connect ()
  in
  try
    match Api.video client ~key video_id with
    | None ->
        Eio.traceln "%s: no such video" video_id
    | Some {live_chat_id= None; _} ->
        Eio.traceln "%s: not live, or chat is disabled" video_id
    | Some {channel; live_chat_id= Some live_chat_id} ->
        listen ~channel ~live_chat_id
  with Api.Api_error {status; reason; message} ->
    Eio.traceln "%s: API error %d %s: %s" video_id status reason message

let ok_or_fail = function Ok x -> x | Error (`Msg m) -> failwith m

let https ~authenticator =
  let tls_config = ok_or_fail (Tls.Config.client ~authenticator ()) in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map ~f:(fun h -> Domain_name.(host_exn (of_string_exn h)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

(* $YOUTUBE_API_KEY, or exits with an error if it isn't set. *)
let api_key env =
  match Sys.getenv "YOUTUBE_API_KEY" with
  | None | Some "" ->
      Eio.Flow.copy_string "youtube: set YOUTUBE_API_KEY\n"
        (Eio.Stdenv.stderr env) ;
      exit 2
  | Some key ->
      key

(* Follows every live stream the [args] refer to until they end, adding each
   chat message to [stream]. Must run inside Eio_main.run with the RNG
   already seeded. *)
let follow_targets env ~key ~stream args =
  let authenticator = ok_or_fail (Ca_certs.authenticator ()) in
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator))
      (Eio.Stdenv.net env)
  in
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.List.iter
    (fun arg ->
      match video_ids ~client ~key arg with
      | ids ->
          Eio.Fiber.List.iter (follow ~clock ~client ~key ~stream) ids
      | exception Api.Api_error {status; reason; message} ->
          Eio.traceln "%s: API error %d %s: %s" arg status reason message )
    args
