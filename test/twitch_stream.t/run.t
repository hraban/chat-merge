Following Twitch chat through EventSub webhooks, against a fake id + Helix
server (fake_twitch.py) that also plays Twitch's side of the webhook.

  $ export TWITCH_CLIENT_ID=cid TWITCH_CLIENT_SECRET=csecret
  $ base=http://127.0.0.1:18081
  $ callback=$base/twitch/eventsub
  $ python3 fake_twitch.py done $callback > port 2>> requests &
  $ while [ ! -s port ]; do sleep 0.05; done
  $ export TWITCH_ID_BASE_URL=http://127.0.0.1:$(cat port)/
  $ export TWITCH_HELIX_BASE_URL=http://127.0.0.1:$(cat port)/

The client removes its stale subscription (but not one for another callback),
subscribes, answers the challenge, and passes on the chat message once. The
replayed and the forged deliveries don't show up.

  $ chat stream --twitch '#Chan' --public-url $base --port 18081 \
  >   > out 2> stderr &
  $ while [ ! -s done ]; do sleep 0.05; done
  $ kill %2
  $ cat out
  T <Alice> hi Kappa
  $ cat stderr
  +listening on port 18081 of 127.0.0.1
  +twitch: removing stale subscription stale (webhook_callback_verification_failed)
  +twitch: webhook verified
  $ cat requests
  POST /token client_credentials cid
  GET /eventsub/subscriptions
  DELETE /eventsub/subscriptions stale
  GET /users chan
  POST /eventsub/subscriptions channel.chat.message broadcaster=100 user=100 webhook secret_len=64
  webhook webhook_callback_verification v1 -> 200 'chal-123'
  webhook notification n1 -> 200 ''
  webhook notification n1 -> 200 ''
  webhook notification n2 -> 403 'bad signature\n'
  $ : > requests

A channel that hasn't authorized the app, and one that doesn't exist: nothing
to read, so the client gives up.

  $ chat stream --twitch locked --public-url $base --port 18081
  +listening on port 18081 of 127.0.0.1
  +twitch: removing stale subscription stale (webhook_callback_verification_failed)
  +#locked: subscription missing proper authorization
  +The app needs channel:bot from the broadcaster and user:read:chat and user:bot from the user it reads as; see "chat twitch-auth-url".
  $ chat stream --twitch nobody --public-url $base --port 18081 \
  >   --twitch-as locked
  +listening on port 18081 of 127.0.0.1
  +twitch: removing stale subscription stale (webhook_callback_verification_failed)
  +#nobody: no such channel

The page and the webhook share one port: without --serve only the webhook is
there, with it the page too.

  $ status() {
  >   python3 -c 'import sys, urllib.request as r, urllib.error as e
  > try: print(r.urlopen(sys.argv[1]).status)
  > except e.HTTPError as x: print(x.code)' "$1"
  > }
  $ rm done ; : > requests
  $ chat stream --twitch chan --public-url $base --port 18081 > /dev/null \
  >   2>&1 &
  $ pid=$!
  $ while [ ! -s done ]; do sleep 0.05; done
  $ status $base/ ; status $base/twitch/eventsub ; status $base/twitch/x
  404
  405
  404
  $ kill $pid ; wait $pid
  [143]
  $ rm done ; : > requests
  $ chat stream --twitch chan --public-url $base --port 18081 --serve \
  >   2> stderr &
  $ pid=$!
  $ while [ ! -s done ]; do sleep 0.05; done
  $ status $base/ ; status $base/app.js ; status $base/twitch/eventsub
  200
  200
  405
  $ kill $pid ; wait $pid
  [143]
  $ cat stderr
  +listening on port 18081 of 127.0.0.1
  +twitch: removing stale subscription stale (webhook_callback_verification_failed)
  +twitch: webhook verified
  $ cat requests
  POST /token client_credentials cid
  GET /eventsub/subscriptions
  DELETE /eventsub/subscriptions stale
  GET /users chan
  POST /eventsub/subscriptions channel.chat.message broadcaster=100 user=100 webhook secret_len=64
  webhook webhook_callback_verification v1 -> 200 'chal-123'
  webhook notification n1 -> 200 ''
  webhook notification n1 -> 200 ''
  webhook notification n2 -> 403 'bad signature\n'

  $ kill %1
