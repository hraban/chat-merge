(* YouTube live chat reader. Follows one or more live streams through the
   official YouTube Data API v3 and prints every chat message to stdout.
   Needs an API key in $YOUTUBE_API_KEY. *)

open Twitch_chat

(* Each poll costs 5 units of the default 10,000/day quota, so never poll
   faster than this even when the server says we may. *)
let min_poll_interval = 5.0

(* Accepts a bare video ID, or a youtube.com/watch?v=, youtube.com/live/ or
   youtu.be/ URL. *)
let video_id_of_arg arg =
  let uri = Uri.of_string arg in
  match (Uri.host uri, Uri.get_query_param uri "v") with
  | None, _ -> arg
  | Some _, Some v -> v
  | Some _, None -> (
      match List.rev (String.split_on_char '/' (Uri.path uri)) with
      | last :: _ when last <> "" -> last
      | _ -> arg)

let print_message out channel (msg : Youtube_api.message) =
  let line = Printf.sprintf "%s <%s> %s" channel msg.author msg.text in
  let line = Sanitize.(apply_filters line output_filters) ^ "\n" in
  Eio.Flow.copy_string line out

(* Follows one stream's chat until it ends. Network and server (5xx) errors
   are retried with the same page token; client (4xx) errors give up. *)
let follow ~clock ~client ~key ~out video_id =
  let rec poll ~channel ~live_chat_id page_token =
    match Youtube_api.messages client ~key ~live_chat_id ?page_token () with
    | page ->
        List.iter (print_message out channel) page.messages;
        if page.offline then Eio.traceln "%s: stream ended" channel
        else (
          Eio.Time.sleep clock (Float.max min_poll_interval page.polling_interval);
          poll ~channel ~live_chat_id page.next_page_token)
    | exception (Youtube_api.Api_error { status; _ } as e) when status < 500 ->
        raise e
    | exception (Eio.Cancel.Cancelled _ as e) -> raise e
    | exception e ->
        Eio.traceln "%s: %s, retrying in %.0fs" channel (Printexc.to_string e)
          min_poll_interval;
        Eio.Time.sleep clock min_poll_interval;
        poll ~channel ~live_chat_id page_token
  in
  try
    match Youtube_api.video client ~key video_id with
    | None -> Eio.traceln "%s: no such video" video_id
    | Some { live_chat_id = None; _ } ->
        Eio.traceln "%s: not live, or chat is disabled" video_id
    | Some { channel; live_chat_id = Some live_chat_id } ->
        poll ~channel ~live_chat_id None
  with Youtube_api.Api_error { status; reason; message } ->
    Eio.traceln "%s: API error %d %s: %s" video_id status reason message

let ok_or_fail = function Ok x -> x | Error (`Msg m) -> failwith m

let https ~authenticator =
  let tls_config = ok_or_fail (Tls.Config.client ~authenticator ()) in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun h -> Domain_name.(host_exn (of_string_exn h)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let stderr = Eio.Stdenv.stderr env in
  match (Sys.getenv_opt "YOUTUBE_API_KEY", List.tl (Array.to_list Sys.argv)) with
  | _, [] ->
      Eio.Flow.copy_string "usage: youtube-chat VIDEO_ID_OR_URL...\n" stderr;
      exit 2
  | (None | Some ""), _ ->
      Eio.Flow.copy_string "youtube-chat: set YOUTUBE_API_KEY\n" stderr;
      exit 2
  | Some key, args ->
      let authenticator = ok_or_fail (Ca_certs.authenticator ()) in
      let client =
        Cohttp_eio.Client.make
          ~https:(Some (https ~authenticator))
          (Eio.Stdenv.net env)
      in
      let clock = Eio.Stdenv.clock env and out = Eio.Stdenv.stdout env in
      Eio.Fiber.List.iter
        (fun arg -> follow ~clock ~client ~key ~out (video_id_of_arg arg))
        args
