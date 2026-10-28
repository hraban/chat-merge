(* The parts of the official YouTube Data API v3 needed to follow a live chat.
   https://developers.google.com/youtube/v3/live/docs/liveChatMessages *)

type error = { status : int; reason : string; message : string }

exception Api_error of error

let base_url = "https://www.googleapis.com/youtube/v3/"

(* Optional lookup through nested objects; [None] on any missing step. *)
let rec find path json =
  match (path, json) with
  | [], j -> Some j
  | key :: rest, `Assoc fields -> Option.bind (List.assoc_opt key fields) (find rest)
  | _ -> None

let find_string path json =
  match find path json with Some (`String s) -> Some s | _ -> None

let get client ~key endpoint params =
  let uri =
    Uri.with_query' (Uri.of_string (base_url ^ endpoint)) (("key", key) :: params)
  in
  Eio.Switch.run @@ fun sw ->
  let resp, body = Cohttp_eio.Client.get ~sw client uri in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:(16 lsl 20) in
  let json =
    try Yojson.Safe.from_string body with Yojson.Json_error _ -> `Null
  in
  match Cohttp.Code.code_of_status (Cohttp.Response.status resp) with
  | 200 -> json
  | status ->
      let reason =
        match find [ "error"; "errors" ] json with
        | Some (`List (e :: _)) -> Option.value (find_string [ "reason" ] e) ~default:""
        | _ -> ""
      in
      let message =
        Option.value (find_string [ "error"; "message" ] json) ~default:body
      in
      raise (Api_error { status; reason; message })

type video = { channel : string; live_chat_id : string option }

(* [None] if no video has that ID. [live_chat_id] is [None] unless the video
   is currently live with chat enabled. Costs 1 quota unit. *)
let video client ~key id =
  let json =
    get client ~key "videos"
      [ ("part", "snippet,liveStreamingDetails"); ("id", id) ]
  in
  match find [ "items" ] json with
  | Some (`List (item :: _)) ->
      Some
        {
          channel =
            Option.value (find_string [ "snippet"; "channelTitle" ] item) ~default:id;
          live_chat_id =
            find_string [ "liveStreamingDetails"; "activeLiveChatId" ] item;
        }
  | _ -> None

(* Channel ID for a handle such as "@name", or [None] if there is no such
   channel. Costs 1 quota unit. *)
let channel_id_of_handle client ~key handle =
  let json = get client ~key "channels" [ ("part", "id"); ("forHandle", handle) ] in
  match find [ "items" ] json with
  | Some (`List (item :: _)) -> find_string [ "id" ] item
  | _ -> None

(* IDs of the channel's videos that are live right now. Search results can
   lag a stream going live by a few minutes. Costs 100 quota units. *)
let live_video_ids client ~key channel_id =
  let json =
    get client ~key "search"
      [
        ("part", "id");
        ("channelId", channel_id);
        ("eventType", "live");
        ("type", "video");
      ]
  in
  match find [ "items" ] json with
  | Some (`List items) -> List.filter_map (find_string [ "id"; "videoId" ]) items
  | _ -> []

type message = { author : string; text : string }

type page = {
  messages : message list;
  next_page_token : string option;
  polling_interval : float;  (** Seconds the server asks us to wait. *)
  offline : bool;  (** The stream has ended; no more messages will come. *)
}

(* One page of chat. Without [page_token] this is the recent backlog; pass the
   previous page's [next_page_token] to get only what arrived since. Only
   plain text messages are returned. Costs 5 quota units. *)
let messages client ~key ~live_chat_id ?page_token () =
  let params =
    [
      ("liveChatId", live_chat_id);
      ("part", "snippet,authorDetails");
      ("maxResults", "2000");
    ]
    @ Option.fold ~none:[] ~some:(fun t -> [ ("pageToken", t) ]) page_token
  in
  let json = get client ~key "liveChat/messages" params in
  let items = match find [ "items" ] json with Some (`List l) -> l | _ -> [] in
  let message item =
    match find_string [ "snippet"; "type" ] item with
    | Some "textMessageEvent" ->
        Some
          {
            author =
              Option.value
                (find_string [ "authorDetails"; "displayName" ] item)
                ~default:"?";
            text =
              Option.value (find_string [ "snippet"; "displayMessage" ] item) ~default:"";
          }
    | _ -> None
  in
  {
    messages = List.filter_map message items;
    next_page_token = find_string [ "nextPageToken" ] json;
    polling_interval =
      (match find [ "pollingIntervalMillis" ] json with
      | Some (`Int ms) -> float_of_int ms /. 1000.
      | _ -> 5.);
    offline = Option.is_some (find [ "offlineAt" ] json);
  }
