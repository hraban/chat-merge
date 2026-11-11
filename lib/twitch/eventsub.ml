(* The receiving end of EventSub webhooks: checks that a request really comes
   from Twitch, answers the callback verification challenge, and turns
   channel.chat.message notifications into chat messages.
   https://dev.twitch.tv/docs/eventsub/handling-webhook-events/ *)

open! Core
open Option.Monad_infix

(* Notifications older than this are refused, as are repeats of one already
   seen, so a captured request can't be replayed. *)
let max_age = Time_ns.Span.of_min 10.

(* How many message IDs to remember. Twitch retries a failed delivery within
   the [max_age] window, so this only needs to cover that many messages. *)
let max_seen = 10_000

type t =
  { secret: string
  ; on_message: Chat_merge.Chat_message.t -> unit
  ; seen: string Hash_set.t
  ; seen_order: string Queue.t }

let create ~secret ~on_message =
  { secret
  ; on_message
  ; seen= Hash_set.create (module String)
  ; seen_order= Queue.create () }

(* The Twitch-Eventsub-Message-Signature header value for a message. *)
let signature ~secret ~id ~timestamp body =
  "sha256="
  ^ Digestif.SHA256.(to_hex (hmac_string ~key:secret (id ^ timestamp ^ body)))

let%expect_test "signature" =
  (* Checked against Python's hmac module. *)
  print_endline
    (signature ~secret:"0123456789" ~id:"msg-1"
       ~timestamp:"2026-11-10T12:00:00.123456789Z" "{}" ) ;
  [%expect
    {| sha256=156977afb4cce0a48954acf058b672573f4ba73b13a9130f549e08dc94ee3e15 |}]

let parse_timestamp s =
  try Some (Time_ns.of_string_with_utc_offset s) with _ -> None

let%expect_test "parse_timestamp" =
  let show s =
    print_s
      [%sexp (parse_timestamp s >>| Time_ns.to_string_utc : string option)]
  in
  show "2026-11-10T12:00:00.123456789Z" ;
  show "2026-11-10T12:00:00Z" ;
  show "yesterday" ;
  [%expect
    {|
    ("2026-11-10 12:00:00.123456789Z")
    ("2026-11-10 12:00:00.000000000Z")
    ()
    |}]

(* Emote IDs end up in a URL, and come from the network. *)
let valid_emote_id id =
  (not (String.is_empty id))
  && String.for_all id ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')

(* "#rrggbb", or [None] when the user never picked one. Checked strictly, as it
   ends up in the page's CSS. *)
let valid_color c =
  if
    String.length c = 7
    && Char.equal c.[0] '#'
    && String.for_all (String.drop_prefix c 1) ~f:Char.is_hex_digit
  then Some (String.lowercase c)
  else None

let message_of_event (e : Helix_json.chat_event) : Chat_merge.Chat_message.t =
  let open Chat_merge.Chat_message in
  let clean = Chat_merge.Sanitize.clean in
  let author =
    List.find ~f:(Fn.non String.is_empty)
      [e.chatter_user_name; e.chatter_user_login]
    |> Option.value ~default:"?"
  in
  let segments =
    match e.message.fragments with
    | [] ->
        plain_segments e.message.text
    | fragments ->
        List.filter_map fragments ~f:(fun (f : Helix_json.fragment) ->
            match f.emote with
            | Some {id}
              when String.equal f.type_ "emote"
                   && valid_emote_id id
                   && not (String.is_empty f.text) ->
                Some (Emote {id; name= clean f.text})
            | _ when String.is_empty f.text ->
                None
            | _ ->
                (* Mentions, cheermotes and the like show as their text. *)
                Some (Text (clean f.text)) )
  in
  { platform= Twitch
  ; channel= "#" ^ e.broadcaster_user_login
  ; author
  ; color= valid_color e.color
  ; text= e.message.text (* EventSub doesn't say whether a message was a /me. *)
  ; kind= Say
  ; segments }

let remember t id =
  Hash_set.add t.seen id ;
  Queue.enqueue t.seen_order id ;
  if Queue.length t.seen_order > max_seen then
    Option.iter (Queue.dequeue t.seen_order) ~f:(Hash_set.remove t.seen)

let decode of_yojson body =
  match of_yojson (Yojson.Safe.from_string body) with
  | x ->
      Some x
  | exception e ->
      Eio.traceln "twitch: undecodable webhook body: %s" (Exn.to_string e) ;
      None

(* Acts on one webhook delivery whose authenticity has been checked. *)
let dispatch t ~message_type body =
  match message_type with
  | "webhook_callback_verification" -> (
    match decode Helix_json.verification_of_yojson body with
    | Some v ->
        Eio.traceln "twitch: webhook verified" ;
        (200, v.challenge)
    | None ->
        (400, "") )
  | "notification" ->
      ( match decode Helix_json.notification_of_yojson body with
      | Some {subscription= {type_= "channel.chat.message"; _}; event} -> (
        match Helix_json.chat_event_of_yojson event with
        | e ->
            t.on_message (message_of_event e)
        | exception e ->
            Eio.traceln "twitch: undecodable chat event: %s" (Exn.to_string e) )
      | Some _ | None ->
          () ) ;
      (200, "")
  | "revocation" ->
      ( match decode Helix_json.notification_of_yojson body with
      | Some {subscription= s; _} ->
          Eio.traceln "twitch: Twitch revoked the %s subscription: %s" s.type_
            s.status
      | None ->
          () ) ;
      (200, "")
  | _ ->
      (200, "")

(* The status and plain text body to answer a webhook POST with. [header]
   looks up a request header, case-insensitively. *)
let handle t ~now ~header body =
  match
    ( header "twitch-eventsub-message-id"
    , header "twitch-eventsub-message-timestamp"
    , header "twitch-eventsub-message-signature"
    , header "twitch-eventsub-message-type" )
  with
  | Some id, Some timestamp, Some given, Some message_type ->
      let expected = signature ~secret:t.secret ~id ~timestamp body in
      if not (Eqaf.equal given expected) then (403, "bad signature\n")
      else if
        match parse_timestamp timestamp with
        | Some at ->
            Time_ns.Span.( > ) (Time_ns.diff now at) max_age
        | None ->
            true
      then (403, "stale message\n")
      else if Hash_set.mem t.seen id then (200, "")
      else (
        remember t id ;
        dispatch t ~message_type body )
  | _ ->
      (400, "not an EventSub message\n")

let%expect_test "handle" =
  let secret = "s3cr3t-s3cr3t" in
  let t =
    create ~secret ~on_message:(fun m ->
        print_endline (Chat_merge.Chat_message.to_line m) )
  in
  let now = Time_ns.of_string_with_utc_offset "2026-11-10T12:00:00Z" in
  let post ?(secret = secret) ?(timestamp = "2026-11-10T11:59:59.5Z") ~id
      message_type body =
    let headers =
      [ ("twitch-eventsub-message-id", id)
      ; ("twitch-eventsub-message-timestamp", timestamp)
      ; ( "twitch-eventsub-message-signature"
        , signature ~secret ~id ~timestamp body )
      ; ("twitch-eventsub-message-type", message_type) ]
    in
    let status, reply =
      handle t ~now
        ~header:(List.Assoc.find headers ~equal:String.Caseless.equal)
        body
    in
    printf "-> %d %S\n" status reply
  in
  post ~id:"1" "webhook_callback_verification"
    {|{"challenge":"pogchamp-kappa-360noscope","subscription":{}}|} ;
  [%expect
    {|
    +twitch: webhook verified
    -> 200 "pogchamp-kappa-360noscope"
    |}] ;
  let chat =
    {|{"subscription":{"type":"channel.chat.message","status":"enabled"},
       "event":{"broadcaster_user_login":"chan","chatter_user_login":"foo",
         "chatter_user_name":"Foo","color":"#8FD3FF","message_type":"text",
         "message":{"text":"hi Kappa @bar",
           "fragments":[
             {"type":"text","text":"hi ","emote":null,"mention":null},
             {"type":"emote","text":"Kappa","emote":{"id":"25","emote_set_id":"0"}},
             {"type":"text","text":" ","emote":null},
             {"type":"mention","text":"@bar","mention":{"user_login":"bar"}}]}}}|}
  in
  post ~id:"2" "notification" chat ;
  [%expect {|
    <Foo> hi Kappa @bar
    -> 200 ""
    |}] ;
  (* A retried delivery is acknowledged but not passed on twice. *)
  post ~id:"2" "notification" chat ;
  [%expect {| -> 200 "" |}] ;
  post ~id:"3" ~secret:"wrong-secret" "notification" chat ;
  [%expect {| -> 403 "bad signature\n" |}] ;
  post ~id:"4" ~timestamp:"2026-11-10T11:49:00Z" "notification" chat ;
  [%expect {| -> 403 "stale message\n" |}] ;
  post ~id:"5" "revocation"
    {|{"subscription":{"type":"channel.chat.message","status":"authorization_revoked"}}|} ;
  [%expect
    {|
    +twitch: Twitch revoked the channel.chat.message subscription: authorization_revoked
    -> 200 ""
    |}] ;
  print_s [%sexp (handle t ~now ~header:(fun _ -> None) "{}" : int * string)] ;
  [%expect {| (400 "not an EventSub message\n") |}]

let%expect_test "message_of_event" =
  let show json =
    let m =
      message_of_event
        (Helix_json.chat_event_of_yojson (Yojson.Safe.from_string json))
    in
    print_endline (Chat_merge.Chat_message.to_line m) ;
    print_s [%sexp (m.color : string option)] ;
    List.iter m.segments ~f:(function
      | Text s ->
          printf "text %S\n" s
      | Emote {id; name} ->
          printf "emote %s %S\n" id name )
  in
  (* No display name, no color, an emote ID that isn't URL-safe, and a
     terminal escape in the text. *)
  show
    {|{"broadcaster_user_login":"chan","chatter_user_login":"foo",
       "chatter_user_name":"","color":"",
       "message":{"text":"x \u001b[2J",
         "fragments":[{"type":"emote","text":"x","emote":{"id":"../x"}},
                      {"type":"text","text":" \u001b[2J"}]}}|} ;
  [%expect
    {|
    <foo> x ⟨U+001B⟩[2J
    ()
    text "x"
    text " \226\159\168U+001B\226\159\169[2J"
    |}] ;
  (* Fragments are optional; the text is used as is. *)
  show
    {|{"broadcaster_user_login":"chan","chatter_user_name":"Bar",
       "color":"red","message":{"text":"plain"}}|} ;
  [%expect {|
    <Bar> plain
    ()
    text "plain"
    |}]
