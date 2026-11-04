(* Serves chat messages to browsers: "/" is a static page, "/events" a
   Server-Sent Events stream of every message that arrives while connected. *)

open! Core

let client_queue_size = 256
let heartbeat_seconds = 15.

let page =
  {|<!doctype html>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>chat</title>
<style>
  :root { color-scheme: light dark; }
  body { font: 15px/1.4 system-ui, sans-serif; margin: 0; padding: 8px 12px; }
  .m { padding: 1px 0; overflow-wrap: anywhere; }
  .twitch .p { color: #9146ff; }
  .youtube .p { color: #e00; }
  #s { position: fixed; top: 4px; right: 8px; opacity: .6; font-size: 12px; }
</style>
<div id="s">connecting…</div>
<div id="log"></div>
<script>
  const log = document.getElementById("log");
  const status = document.getElementById("s");
  const es = new EventSource("events");
  es.onopen = () => status.textContent = "live";
  es.onerror = () => status.textContent = "reconnecting…";
  es.onmessage = (e) => {
    const m = JSON.parse(e.data);
    const stick = innerHeight + scrollY >= document.body.scrollHeight - 40;
    const div = document.createElement("div");
    div.className = "m " + m.platform;
    const p = document.createElement("span");
    p.className = "p";
    p.textContent = "● ";
    div.append(p, m.line); // textContent semantics: never parsed as HTML
    log.append(div);
    while (log.childElementCount > 1000) log.firstChild.remove();
    if (stick) scrollTo(0, document.body.scrollHeight);
  };
</script>
|}

let platform_name : Chat_message.platform -> string = function
  | Twitch -> "twitch"
  | Youtube -> "youtube"

(* The SSE event for one message. [to_line] already sanitizes, and the page
   only ever inserts [line] as text. *)
let event_of_message (m : Chat_message.t) =
  let json =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("platform", `String (platform_name m.platform));
          ("line", `String (Chat_message.to_line m));
        ])
  in
  sprintf "data: %s\n\n" json

let%expect_test "event_of_message" =
  print_string
    (event_of_message
       {
         platform = Twitch;
         channel = "#c";
         author = "a";
         text = "<script>\"hi\"";
         kind = Say;
       });
  [%expect {|
    data: {"platform":"twitch","line":"#c <a> <script>\"hi\""}

    |}]

(* One queue per connected browser, so a slow visitor can't hold up the rest. *)
type t = { mutable clients : string Eio.Stream.t list }

let broadcast t event =
  List.iter t.clients ~f:(fun q ->
      (* Single domain: no one can fill the queue between check and add, so
         [add] never blocks. A visitor that lags this far behind just misses
         messages. *)
      if Eio.Stream.length q < client_queue_size then Eio.Stream.add q event)

(* Chunked transfer encoding by hand: the stock body writer doesn't flush after
   each chunk, which would hold messages back until a buffer fills. *)
let write_chunk oc s =
  Eio.Buf_write.printf oc "%x\r\n%s\r\n" (String.length s) s;
  Eio.Buf_write.flush oc

let serve_events t env oc =
  let q = Eio.Stream.create client_queue_size in
  t.clients <- q :: t.clients;
  Exn.protect
    ~finally:(fun () -> t.clients <- List.filter t.clients ~f:(fun c -> not (phys_equal c q)))
    ~f:(fun () ->
      write_chunk oc ": connected\n\n";
      let clock = Eio.Stdenv.clock env in
      let rec loop () =
        (match
           Eio.Time.with_timeout clock heartbeat_seconds (fun () ->
               Ok (Eio.Stream.take q))
         with
        | Ok event -> write_chunk oc event
        | Error `Timeout ->
            (* Also how we notice a vanished browser: the write fails. *)
            write_chunk oc ": keepalive\n\n");
        loop ()
      in
      loop ())

let callback t env _conn (req : Http.Request.t) _body =
  let headers = Http.Header.of_list in
  match (req.meth, Uri.path (Uri.of_string req.resource)) with
  | `GET, "/" ->
      `Expert
        ( Http.Response.make ~status:`OK
            ~headers:
              (headers
                 [
                   ("content-type", "text/html; charset=utf-8");
                   ("content-length", Int.to_string (String.length page));
                   ("connection", "close");
                   ( "content-security-policy",
                     "default-src 'none'; script-src 'unsafe-inline'; \
                      style-src 'unsafe-inline'; connect-src 'self'" );
                 ])
            (),
          fun _ic oc ->
            Eio.Buf_write.string oc page;
            Eio.Buf_write.flush oc )
  | `GET, "/events" ->
      `Expert
        ( Http.Response.make ~status:`OK
            ~headers:
              (headers
                 [
                   ("content-type", "text/event-stream");
                   ("cache-control", "no-cache");
                   ("transfer-encoding", "chunked");
                   ("connection", "close");
                 ])
            (),
          fun _ic oc -> serve_events t env oc )
  | _ ->
      let body = "not found\n" in
      `Expert
        ( Http.Response.make ~status:`Not_found
            ~headers:
              (headers
                 [
                   ("content-length", Int.to_string (String.length body));
                   ("connection", "close");
                 ])
            (),
          fun _ic oc ->
            Eio.Buf_write.string oc body;
            Eio.Buf_write.flush oc )

(* Serves on [addr:port] and forwards every message taken from [stream] to all
   connected browsers. Runs until cancelled. *)
let run env ~addr ~port ~(stream : Chat_message.t Eio.Stream.t) =
  Eio.Switch.run @@ fun sw ->
  let t = { clients = [] } in
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~backlog:32
      (`Tcp (addr, port))
  in
  let server =
    Cohttp_eio.Server.make_response_action
      ~callback:(fun conn req body -> callback t env conn req body)
      ()
  in
  Eio.Fiber.first
    (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
          Eio.traceln "web: %s" (Exn.to_string exn)))
    (fun () ->
      let rec loop () =
        broadcast t (event_of_message (Eio.Stream.take stream));
        loop ()
      in
      loop ())
