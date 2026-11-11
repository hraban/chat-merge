"""Fake Twitch id + Helix API for exercising the EventSub webhook client.

Listens on a free port on 127.0.0.1, prints the port number to stdout, then
logs each request it gets, and each webhook it delivers, to stderr.

Users: "chan" (id 100) and "locked" (id 403, whose subscription is refused
as unauthorized). Any other login does not exist.

On a chat subscription it plays Twitch: it posts the verification challenge
to the callback, then a chat notification, the same notification again, one
with a bad signature, and finally writes "done" to the file named in argv[1].
"""

import hashlib
import hmac
import json
import sys
import threading
import urllib.request
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlparse

USERS = {"chan": "100", "locked": "403"}
done_file = sys.argv[1]


def log(*a):
    print(*a, file=sys.stderr, flush=True)


def deliver(callback, secret, msg_id, msg_type, body, sign_with=None):
    body = json.dumps(body).encode()
    ts = datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")
    mac = hmac.new((sign_with or secret).encode(),
                   msg_id.encode() + ts.encode() + body, hashlib.sha256)
    req = urllib.request.Request(callback, data=body, method="POST", headers={
        "Twitch-Eventsub-Message-Id": msg_id,
        "Twitch-Eventsub-Message-Timestamp": ts,
        "Twitch-Eventsub-Message-Signature": "sha256=" + mac.hexdigest(),
        "Twitch-Eventsub-Message-Type": msg_type,
        "Content-Type": "application/json",
    })
    try:
        with urllib.request.urlopen(req) as r:
            status, reply = r.status, r.read().decode()
    except urllib.error.HTTPError as e:
        status, reply = e.code, e.read().decode()
    log(f"webhook {msg_type} {msg_id} -> {status} {reply!r}")


def play(sub):
    t = sub["transport"]
    cb, secret = t["callback"], t["secret"]
    s = {"type": sub["type"], "status": "enabled"}
    deliver(cb, secret, "v1", "webhook_callback_verification",
            {"challenge": "chal-123", "subscription": s})
    chat = {"subscription": s, "event": {
        "broadcaster_user_login": "chan",
        "chatter_user_login": "alice",
        "chatter_user_name": "Alice",
        "color": "#1E90FF",
        "message_type": "text",
        "message": {"text": "hi Kappa", "fragments": [
            {"type": "text", "text": "hi ", "emote": None},
            {"type": "emote", "text": "Kappa", "emote": {"id": "25"}},
        ]},
    }}
    deliver(cb, secret, "n1", "notification", chat)
    deliver(cb, secret, "n1", "notification", chat)
    deliver(cb, secret, "n2", "notification", chat, sign_with="not the secret")
    with open(done_file, "w") as f:
        f.write("done\n")


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *a):
        pass

    def reply(self, status, obj=None):
        b = json.dumps(obj).encode() if obj is not None else b""
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        self.wfile.write(b)

    def body(self):
        return self.rfile.read(int(self.headers.get("Content-Length", 0)))

    def query(self):
        u = urlparse(self.path)
        return u.path, parse_qs(u.query)

    def authorized(self):
        return (self.headers.get("Authorization") == "Bearer app-token"
                and self.headers.get("Client-Id") == "cid")

    def do_POST(self):
        path, _ = self.query()
        body = self.body()
        if path == "/token":
            form = parse_qs(body.decode())
            log("POST /token", form["grant_type"][0], form["client_id"][0])
            return self.reply(200, {"access_token": "app-token",
                                    "expires_in": 5000000,
                                    "token_type": "bearer"})
        if path == "/eventsub/subscriptions" and self.authorized():
            sub = json.loads(body)
            c = sub["condition"]
            log("POST /eventsub/subscriptions", sub["type"],
                f"broadcaster={c['broadcaster_user_id']} user={c['user_id']}",
                sub["transport"]["method"],
                f"secret_len={len(sub['transport']['secret'])}")
            if c["broadcaster_user_id"] == "403":
                return self.reply(403, {
                    "error": "Forbidden", "status": 403,
                    "message": "subscription missing proper authorization"})
            self.reply(202, {"data": [{"id": "new", "status":
                             "webhook_callback_verification_pending"}]})
            threading.Thread(target=play, args=(sub,)).start()
            return
        self.reply(404, {"error": "Not Found", "status": 404, "message": path})

    def do_GET(self):
        path, q = self.query()
        if not self.authorized():
            return self.reply(401, {"status": 401, "message": "bad token"})
        if path == "/users":
            logins = q.get("login", [])
            log("GET /users", " ".join(logins))
            return self.reply(200, {"data": [
                {"id": USERS[l], "login": l} for l in logins if l in USERS]})
        if path == "/eventsub/subscriptions":
            log("GET /eventsub/subscriptions")
            cb = self.server.callback
            return self.reply(200, {"data": [
                {"id": "stale", "status": "webhook_callback_verification_failed",
                 "type": "channel.chat.message",
                 "transport": {"method": "webhook", "callback": cb}},
                {"id": "other", "status": "enabled",
                 "type": "channel.chat.message",
                 "transport": {"method": "webhook",
                               "callback": "https://elsewhere.example/"}},
            ], "pagination": {}})
        self.reply(404, {"status": 404, "message": path})

    def do_DELETE(self):
        path, q = self.query()
        log("DELETE", path, q["id"][0])
        self.reply(204)


server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
server.callback = sys.argv[2]
print(server.server_address[1], flush=True)
server.serve_forever()
