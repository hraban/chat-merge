(* The one HTTP listener, and the chat page it serves: "/" and its stylesheet
   and script are static files, "/events" a Server-Sent Events stream of every
   message that arrives while connected. Other parts (Twitch webhooks) mount
   their own routes next to it. *)

open! Core

let client_queue_size = 256

let heartbeat_seconds = 15.

(* Static files, compiled into the binary: request path -> content type, body. *)
let assets =
  [ ("/", ("text/html; charset=utf-8", [%blob "assets/index.html"]))
  ; ("/app.js", ("text/javascript; charset=utf-8", [%blob "assets/app.js"]))
  ; ( "/robots.txt"
    , ("text/plain; charset=us-ascii", [%blob "assets/robots.txt"]) )
  ; ("/style.css", ("text/css; charset=utf-8", [%blob "assets/style.css"])) ]

let platform_name : Chat_message.platform -> string = function
  | Twitch ->
      "twitch"
  | Youtube ->
      "youtube"

(* The SSE event for one message. [author] and [segments] are sanitized, and
   the page only ever inserts their text as text. *)
let event_of_message (m : Chat_message.t) =
  let json =
    Yojson.Safe.to_string
      (`Assoc
         [ ("platform", `String (platform_name m.platform))
         ; ("author", `String (Sanitize.clean m.author))
         ; ( "color"
           , Option.value_map m.color ~default:`Null ~f:(fun c -> `String c) )
         ; ( "kind"
           , `String (match m.kind with Say -> "say" | Action -> "action") )
         ; ( "segments"
           , `List
               (List.map m.segments ~f:(function
                 | Chat_message.Text text ->
                     `Assoc [("text", `String text)]
                 | Chat_message.Emote {id; name} ->
                     `Assoc [("emote", `String id); ("text", `String name)] ))
           ) ] )
  in
  sprintf "data: %s\n\n" json

let%expect_test "event_of_message" =
  print_string
    (event_of_message
       { platform= Twitch
       ; channel= "#c"
       ; author= "a"
       ; color= Some "#1e90ff"
       ; text= "<script>\"hi\""
       ; kind= Say
       ; segments= [Emote {id= "25"; name= "<"}; Text "script>\"hi\""] } ) ;
  [%expect
    {| data: {"platform":"twitch","author":"a","color":"#1e90ff","kind":"say","segments":[{"emote":"25","text":"<"},{"text":"script>\"hi\""}]} |}]

(* One queue per connected browser, so a slow visitor can't hold up the rest. *)
type t = {mutable clients: string Eio.Stream.t list}

let create () = {clients= []}

(* Answers one request; see [serve]. *)
type handler =
     Cohttp_eio.Server.conn
  -> Http.Request.t
  -> Cohttp_eio.Server.body
  -> Cohttp_eio.Server.response_action

let broadcast t event =
  List.iter t.clients ~f:(fun q ->
      (* Single domain: no one can fill the queue between check and add, so
         [add] never blocks. A visitor that lags this far behind just misses
         messages. *)
      if Eio.Stream.length q < client_queue_size then Eio.Stream.add q event )

(* Chunked transfer encoding by hand: the stock body writer doesn't flush after
   each chunk, which would hold messages back until a buffer fills. *)
let write_chunk oc s =
  Eio.Buf_write.printf oc "%x\r\n%s\r\n" (String.length s) s ;
  Eio.Buf_write.flush oc

let serve_events t env oc =
  let q = Eio.Stream.create client_queue_size in
  t.clients <- q :: t.clients ;
  Exn.protect
    ~finally:(fun () ->
      t.clients <- List.filter t.clients ~f:(fun c -> not (phys_equal c q)) )
    ~f:(fun () ->
      write_chunk oc ": connected\n\n" ;
      let clock = Eio.Stdenv.clock env in
      let rec loop () =
        ( match
            Eio.Time.with_timeout clock heartbeat_seconds (fun () ->
                Ok (Eio.Stream.take q) )
          with
        | Ok event ->
            write_chunk oc event
        | Error `Timeout ->
            (* Also how we notice a vanished browser: the write fails. *)
            write_chunk oc ": keepalive\n\n" ) ;
        loop ()
      in
      loop () )

let respond ~status ~headers body =
  `Expert
    ( Http.Response.make ~status
        ~headers:
          (Http.Header.of_list
             ( ("content-length", Int.to_string (String.length body))
             :: ("connection", "close") :: headers ) )
        ()
    , fun _ic oc ->
        Eio.Buf_write.string oc body ;
        Eio.Buf_write.flush oc )

let not_found : handler =
 fun _conn _req _body -> respond ~status:`Not_found ~headers:[] "not found\n"

(* The chat page. Answers 404 for any path that isn't part of it. *)
let handler t env : handler =
 fun conn (req : Http.Request.t) body ->
  match (req.meth, Uri.path (Uri.of_string req.resource)) with
  | `GET, "/events" ->
      `Expert
        ( Http.Response.make ~status:`OK
            ~headers:
              (Http.Header.of_list
                 [ ("content-type", "text/event-stream")
                 ; ("cache-control", "no-cache")
                 ; ("transfer-encoding", "chunked")
                 ; ("connection", "close") ] )
            ()
        , fun _ic oc -> serve_events t env oc )
  | `GET, path -> (
    match List.Assoc.find assets path ~equal:String.equal with
    | Some (content_type, body) ->
        respond ~status:`OK
          ~headers:
            [ ("content-type", content_type)
              (* Revalidate every time, so a rebuilt binary is never paired
                 with a stale script or stylesheet. *)
            ; ("cache-control", "no-cache")
            ; ( "content-security-policy"
              , "default-src 'none'; script-src 'self'; style-src 'self'; \
                 connect-src 'self'; img-src https://static-cdn.jtvnw.net" ) ]
          body
    | None ->
        not_found conn req body )
  | _ ->
      not_found conn req body

(* Forwards every message taken from [stream] to all connected browsers. Runs
   until cancelled. *)
let pump t (stream : Chat_message.t Eio.Stream.t) =
  let rec loop () =
    broadcast t (event_of_message (Eio.Stream.take stream)) ;
    loop ()
  in
  loop ()

(* Answers requests on [socket] until cancelled: those for a path in [routes]
   with its handler, all others with [default]. *)
let serve ~socket ~routes ~(default : handler) =
  let callback conn (req : Http.Request.t) body =
    let path = Uri.path (Uri.of_string req.resource) in
    let h =
      List.Assoc.find routes ~equal:String.equal path |> Option.value ~default
    in
    h conn req body
  in
  Cohttp_eio.Server.run socket
    (Cohttp_eio.Server.make_response_action ~callback ()) ~on_error:(fun exn ->
      Eio.traceln "web: %s" (Exn.to_string exn) )
