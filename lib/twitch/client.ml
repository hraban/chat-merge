(* Twitch chat reader on the Helix API. Subscribes to channel.chat.message
   through EventSub with an app access token, and receives the messages as
   webhooks on a local HTTP listener that must be reachable from Twitch as
   [callback], an HTTPS URL on port 443 (e.g. through a reverse proxy or a
   tunnel).

   An app token only reads the chat of channels whose broadcaster has granted
   the app channel:bot, as read by a user who has granted it user:read:chat
   and user:bot; see [Helix.authorize_url]. *)

open! Core

type config =
  { client_id: string
  ; client_secret: string
  ; callback: string  (** Public HTTPS URL that reaches the listener. *)
  ; listen: Eio.Net.Sockaddr.stream  (** Where the listener accepts. *)
  ; read_as: string option
        (** Login of the user to read chat as; the broadcaster if [None]. *) }

let getenv_exn env var =
  match Sys.getenv var with
  | None | Some "" ->
      Eio.Flow.copy_string
        (sprintf "twitch: set %s\n" var)
        (Eio.Stdenv.stderr env) ;
      exit 2
  | Some v ->
      v

(* $TWITCH_CLIENT_ID and $TWITCH_CLIENT_SECRET, or exits with an error if
   either isn't set. *)
let credentials env =
  (getenv_exn env "TWITCH_CLIENT_ID", getenv_exn env "TWITCH_CLIENT_SECRET")

let normalize_login c =
  String.chop_prefix_if_exists (String.lowercase c) ~prefix:"#"

(* Answers webhook POSTs on any path: the signature check is what keeps
   strangers out, and a reverse proxy may rewrite the path. *)
let serve_webhook env ~sw ~listen eventsub =
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~backlog:32 listen
  in
  let clock = Eio.Stdenv.clock env in
  let callback _conn (req : Http.Request.t) body =
    match req.meth with
    | `POST ->
        let body =
          Eio.Buf_read.(parse_exn take_all) body ~max_size:(1 lsl 20)
        in
        let now =
          Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (Eio.Time.now clock))
        in
        let status, reply =
          Eventsub.handle eventsub ~now
            ~header:(Http.Header.get req.headers)
            body
        in
        Cohttp_eio.Server.respond_string
          ~headers:(Http.Header.of_list [("content-type", "text/plain")])
          ~status:(Http.Status.of_int status)
          ~body:reply ()
    | _ ->
        Cohttp_eio.Server.respond_string ~status:`Method_not_allowed
          ~body:"POST only\n" ()
  in
  fun () ->
    Cohttp_eio.Server.run socket (Cohttp_eio.Server.make ~callback ())
      ~on_error:(fun exn ->
        Eio.traceln "twitch: webhook: %s" (Exn.to_string exn) )

(* Our chat subscriptions left over from earlier runs. Their secret is gone
   with the process that made them, so they can only get in the way: Twitch
   refuses a duplicate subscription. *)
let remove_stale_subscriptions helix ~callback =
  Helix.subscriptions helix
  |> List.filter ~f:(fun (s : Helix_json.subscription) ->
      String.equal s.type_ "channel.chat.message"
      && [%equal: string option] s.transport.callback (Some callback) )
  |> List.iter ~f:(fun (s : Helix_json.subscription) ->
      Eio.traceln "twitch: removing stale subscription %s (%s)" s.id s.status ;
      Helix.delete_subscription helix s.id )

(* Subscribes to every channel that exists and lets the app read it. Returns
   how many that is. *)
let subscribe_all helix ~config ~secret channels =
  let ids = Helix.user_ids helix (Option.to_list config.read_as @ channels) in
  let id_of login = List.Assoc.find ids ~equal:String.equal login in
  match config.read_as with
  | Some login when Option.is_none (id_of login) ->
      Eio.traceln "twitch: --twitch-as %s: no such user" login ;
      0
  | _ ->
      List.count channels ~f:(fun channel ->
          match id_of channel with
          | None ->
              Eio.traceln "#%s: no such channel" channel ;
              false
          | Some broadcaster_id -> (
              let user_id =
                Option.value
                  (Option.bind config.read_as ~f:id_of)
                  ~default:broadcaster_id
              in
              match
                Helix.subscribe_chat helix ~broadcaster_id ~user_id
                  ~callback:config.callback ~secret
              with
              | () ->
                  true
              | exception Helix.Api_error {status= 403; message} ->
                  Eio.traceln
                    "#%s: %s\n\
                     The app needs channel:bot from the broadcaster and \
                     user:read:chat and user:bot from the user it reads as; \
                     see \"chat twitch-auth-url\"."
                    channel message ;
                  false
              | exception Helix.Api_error {status; message} ->
                  Eio.traceln "#%s: Helix error %d: %s" channel status message ;
                  false ) )

(* Reads [channels] until cancelled, adding each chat message to [stream].
   Returns at once if none can be read. Must run inside Eio_main.run with the
   RNG already seeded. *)
let follow env ~config ~stream channels =
  let channels =
    List.map channels ~f:normalize_login
    |> List.dedup_and_sort ~compare:String.compare
  in
  let config =
    {config with read_as= Option.map config.read_as ~f:normalize_login}
  in
  (* Only Twitch and we know it; it signs every webhook delivery. *)
  let secret =
    String.concat_map (Mirage_crypto_rng.generate 32) ~f:(fun c ->
        sprintf "%02x" (Char.to_int c) )
  in
  let eventsub = Eventsub.create ~secret ~on_message:(Eio.Stream.add stream) in
  try
    let helix =
      Helix.connect (Helix.https_client env) ~client_id:config.client_id
        ~client_secret:config.client_secret
    in
    Eio.Switch.run
    @@ fun sw ->
    (* Listening before subscribing: Twitch posts the verification challenge
       right away. *)
    let server = serve_webhook env ~sw ~listen:config.listen eventsub in
    Eio.Fiber.first server (fun () ->
        remove_stale_subscriptions helix ~callback:config.callback ;
        if subscribe_all helix ~config ~secret channels > 0 then
          Eio.Fiber.await_cancel () )
  with Helix.Api_error {status; message} ->
    Eio.traceln "twitch: Helix error %d: %s" status message
