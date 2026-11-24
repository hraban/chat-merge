Following Twitch chat through EventSub webhooks, against a fake id + Helix
server (fake_twitch.py) that also plays Twitch's side of the webhook.

  $ export TWITCH_CLIENT_ID=cid TWITCH_CLIENT_SECRET=csecret
  $ callback=http://127.0.0.1:18081/eventsub
  $ python3 fake_twitch.py done $callback > port 2>> requests &
  $ while [ ! -s port ]; do sleep 0.05; done
  $ export TWITCH_ID_BASE_URL=http://127.0.0.1:$(cat port)/
  $ export TWITCH_HELIX_BASE_URL=http://127.0.0.1:$(cat port)/

The client removes its stale subscription (but not one for another callback),
subscribes, answers the challenge, and passes on the chat message once. The
replayed and the forged deliveries don't show up.

  $ chat stream --twitch '#Chan' --twitch-callback $callback --twitch-port 18081 \
  >   > out 2> stderr &
  $ while [ ! -s done ]; do sleep 0.05; done
  $ kill %2
  $ cat out
  T <Alice> hi Kappa
  $ cat stderr
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

  $ chat stream --twitch locked --twitch-callback $callback --twitch-port 18081
  +twitch: removing stale subscription stale (webhook_callback_verification_failed)
  +#locked: subscription missing proper authorization
  +The app needs channel:bot from the broadcaster and user:read:chat and user:bot from the user it reads as; see "chat twitch-auth-url".
  $ chat stream --twitch nobody --twitch-callback $callback --twitch-port 18081 \
  >   --twitch-as locked
  +twitch: removing stale subscription stale (webhook_callback_verification_failed)
  +#nobody: no such channel

Port already in use:

  $ chat stream --twitch chan --twitch-callback $callback --twitch-port 18081 \
  >   --serve --port 18081
  +serving on port 18081 of 127.0.0.1
  stream: --twitch-port: 127.0.0.1 port 18081 already in use
  [2]

  $ kill %1
