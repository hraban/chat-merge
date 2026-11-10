"""Fake YouTube Data API for exercising the liveChat/messages/stream client.

Listens on a free port on 127.0.0.1, prints the port number to stdout, then
logs each request (minus the API key) to stderr.

Video IDs select scenarios (the live chat ID is "chat-" + video ID):
  ok      two connections: the first closes cleanly after a page with a token;
          the second must resume from that token and ends with offlineAt
  cut     the first connection is cut mid-array; the reconnect must resume
          from the last token
  midErr  a 200 stream that sends one page and then an error element
  hdrErr  a 403 whose error body is wrapped in an array
"""

import json
import sys
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlparse

# Pause between stream elements, long enough that the client has to handle
# each element as it arrives rather than the whole body at once.
DELAY = 0.2


def log(*a):
    print(*a, file=sys.stderr, flush=True)


def msg(author, text):
    return {
        "snippet": {"type": "textMessageEvent", "displayMessage": text},
        "authorDetails": {"displayName": author},
    }


def page(msgs, token=None, offline=False):
    p = {"kind": "youtube#liveChatMessageListResponse", "items": msgs}
    if token:
        p["nextPageToken"] = token
    if offline:
        p["offlineAt"] = "2026-11-10T00:00:00Z"
    return p


def err(code, reason, message):
    return {
        "error": {
            "code": code,
            "message": message,
            "errors": [{"message": message, "domain": "global", "reason": reason}],
        }
    }


connections = {}


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *a):
        pass

    def send_body(self, status, body):
        b = body.encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        self.wfile.write(b)

    def chunk(self, s):
        b = s.encode()
        self.wfile.write(f"{len(b):x}\r\n".encode() + b + b"\r\n")
        self.wfile.flush()

    # Sends [elems] the way Google frames a server stream: one JSON array,
    # each element in its own chunk, with a pause in between.
    def stream(self, elems, cut=False):
        self.send_response(200)
        self.send_header("Content-Type", "application/json; charset=UTF-8")
        self.send_header("Transfer-Encoding", "chunked")
        self.end_headers()
        self.close_connection = True
        try:
            for i, e in enumerate(elems):
                if i:
                    time.sleep(DELAY)
                self.chunk(("[" if i == 0 else "\n,") + json.dumps(e, indent=1))
            if cut:
                self.connection.shutdown(2)
                return
            time.sleep(DELAY)
            self.chunk("\n]")
            self.wfile.write(b"0\r\n\r\n")
            self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            # The client hangs up once it sees offlineAt or an error.
            pass

    def do_GET(self):
        u = urlparse(self.path)
        q = {k: v[0] for k, v in parse_qs(u.query).items()}
        q.pop("key", None)
        log("GET", u.path, " ".join(f"{k}={v}" for k, v in sorted(q.items())))
        if u.path.endswith("/videos"):
            vid = q["id"]
            return self.send_body(200, json.dumps({"items": [{
                "snippet": {"channelTitle": "chan-" + vid},
                "liveStreamingDetails": {"activeLiveChatId": "chat-" + vid},
            }]}))
        if u.path.endswith("/liveChat/messages/stream"):
            scenario = q["liveChatId"].removeprefix("chat-")
            n = connections[scenario] = connections.get(scenario, 0) + 1
            token = q.get("pageToken")
            if scenario == "ok" and n == 1:
                return self.stream([
                    page([msg("alice", "hi"), msg("bob", 'a "quoted" } brace')]),
                    page([msg("carol", "third")], token="t1"),
                ])
            if scenario == "ok" and token == "t1":
                return self.stream([
                    page([msg("dave", "after reconnect")], token="t2"),
                    page([], token="t3", offline=True),
                ])
            if scenario == "cut" and n == 1:
                return self.stream([page([msg("eve", "before cut")], token="c1")],
                                   cut=True)
            if scenario == "cut" and token == "c1":
                return self.stream([
                    page([msg("frank", "resumed")], token="c2", offline=True),
                ])
            if scenario == "midErr":
                return self.stream([
                    page([msg("gina", "before error")], token="m1"),
                    err(403, "liveChatEnded", "The live chat is no longer live."),
                ])
            if scenario == "hdrErr":
                body = json.dumps(err(403, "forbidden", "header error"), indent=1)
                return self.send_body(403, "[" + body + "\n]")
        self.send_body(404, json.dumps(err(404, "notFound", "unexpected request")))


server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
