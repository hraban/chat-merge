(* The parts of the Twitch Helix API needed to read chat through EventSub
   webhooks, using an app access token (client credentials grant).
   https://dev.twitch.tv/docs/api/reference/ *)

open! Core
open Option.Monad_infix

let ( let+ ) = ( >>| )

type error = {status: int; message: string}

exception Api_error of error

(* $TWITCH_HELIX_BASE_URL and $TWITCH_ID_BASE_URL replace these, so tests can
   talk to a fake server. *)
let base_url var default =
  match Sys.getenv var with None | Some "" -> default | Some url -> url

let helix_url = base_url "TWITCH_HELIX_BASE_URL" "https://api.twitch.tv/helix/"

let id_url = base_url "TWITCH_ID_BASE_URL" "https://id.twitch.tv/oauth2/"

type t = {http: Cohttp_eio.Client.t; client_id: string; access_token: string}

let ok_or_fail = function Ok x -> x | Error (`Msg m) -> failwith m

let https_client env =
  let authenticator = ok_or_fail (Ca_certs.authenticator ()) in
  let tls_config = ok_or_fail (Tls.Config.client ~authenticator ()) in
  let https uri raw =
    let host =
      let+ h = Uri.host uri in
      Domain_name.(host_exn (of_string_exn h))
    in
    Tls_eio.client_of_flow ?host tls_config raw
  in
  Cohttp_eio.Client.make ~https:(Some https) (Eio.Stdenv.net env)

let api_error ~status body =
  let message =
    try (Helix_json.error_of_yojson (Yojson.Safe.from_string body)).message
    with _ -> body
  in
  Api_error
    {status; message= (if String.is_empty message then body else message)}

(* Sends the request and returns the status and body of a 2xx response; any
   other status is an [Api_error]. *)
let call http ?(headers = []) ?body meth uri =
  Eio.Switch.run
  @@ fun sw ->
  let body = Option.map body ~f:Cohttp_eio.Body.of_string in
  let resp, body =
    Cohttp_eio.Client.call ~sw http meth uri ?body
      ~headers:(Http.Header.of_list headers)
  in
  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:(16 lsl 20) in
  match Cohttp.Code.code_of_status (Cohttp.Response.status resp) with
  | status when status >= 200 && status < 300 ->
      (status, body)
  | status ->
      raise (api_error ~status body)

let decode of_yojson body =
  try of_yojson (Yojson.Safe.from_string body)
  with e -> raise (Api_error {status= 200; message= Exn.to_string e})

(* Logs in as the app itself. The token lasts for weeks, longer than a stream. *)
let connect http ~client_id ~client_secret =
  let _, body =
    call http `POST
      ~headers:[("content-type", "application/x-www-form-urlencoded")]
      ~body:
        (Uri.encoded_of_query
           [ ("client_id", [client_id])
           ; ("client_secret", [client_secret])
           ; ("grant_type", ["client_credentials"]) ] )
      (Uri.of_string (id_url ^ "token"))
  in
  let token = decode Helix_json.token_of_yojson body in
  {http; client_id; access_token= token.access_token}

let request t ?body meth endpoint params =
  let headers =
    [("client-id", t.client_id); ("authorization", "Bearer " ^ t.access_token)]
    @ if Option.is_some body then [("content-type", "application/json")] else []
  in
  call t.http ~headers ?body meth
    (Uri.with_query (Uri.of_string (helix_url ^ endpoint)) params)

(* Login -> user ID, for those logins that exist. *)
let user_ids t logins =
  if List.is_empty logins then []
  else
    let _, body =
      request t `GET "users"
        (* One login=... parameter each; Uri would join a list with commas. *)
        (List.map (List.dedup_and_sort logins ~compare:String.compare)
           ~f:(fun l -> ("login", [l]) ) )
    in
    (decode Helix_json.users_of_yojson body).data
    |> List.map ~f:(fun (u : Helix_json.user) -> (u.login, u.id))

let subscriptions t =
  let rec go after acc =
    let _, body =
      request t `GET "eventsub/subscriptions"
        (Option.value_map after ~default:[] ~f:(fun c -> [("after", [c])]))
    in
    let r = decode Helix_json.subscriptions_of_yojson body in
    let acc = acc @ r.data in
    match r.pagination >>= fun p -> p.cursor with
    | Some c when not (List.is_empty r.data) ->
        go (Some c) acc
    | _ ->
        acc
  in
  go None []

let delete_subscription t id =
  ignore
    (request t `DELETE "eventsub/subscriptions" [("id", [id])] : int * string)

(* Asks Twitch to post [broadcaster_id]'s chat, as read by [user_id], to the
   webhook at [callback], signed with [secret]. Twitch first posts a
   verification challenge to [callback], which must already be answering. *)
let subscribe_chat t ~broadcaster_id ~user_id ~callback ~secret =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [ ("type", `String "channel.chat.message")
         ; ("version", `String "1")
         ; ( "condition"
           , `Assoc
               [ ("broadcaster_user_id", `String broadcaster_id)
               ; ("user_id", `String user_id) ] )
         ; ( "transport"
           , `Assoc
               [ ("method", `String "webhook")
               ; ("callback", `String callback)
               ; ("secret", `String secret) ] ) ] )
  in
  ignore (request t ~body `POST "eventsub/subscriptions" [] : int * string)

(* Where a user grants the app the scopes it needs to read chat with an app
   token: user:read:chat and user:bot for the user chat is read as, and
   channel:bot for the broadcaster (one person, when reading your own chat).
   [redirect_uri] must be one registered for the app. *)
let authorize_url ~client_id ~redirect_uri =
  Uri.to_string
    (Uri.with_query'
       (Uri.of_string "https://id.twitch.tv/oauth2/authorize")
       [ ("client_id", client_id)
       ; ("redirect_uri", redirect_uri)
       ; ("response_type", "token")
       ; ("scope", "user:read:chat user:bot channel:bot") ] )
