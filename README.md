# Chat-merge: display youtube and twitch chat in same session

Mostly AI generated project to merge twitch and youtube live chat sessions into a single session.  Spin up a server, add a web scene in OBS and add it as an overlay to have a diy-multi stream chat overlay.

This project is written in OCaml and heavily uses janestreet core where applicable.

Envvars:

- `YOUTUBE_API_KEY`
- `TWITCH_CLIENT_ID`
- `TWITCH_CLIENT_SECRET`

## Twitch

Twitch chat is read through the Helix API: an EventSub `channel.chat.message`
subscription with an app access token (client credentials), delivered as
webhooks. That needs:

1. A Twitch app (dev.twitch.tv console) whose id and secret are in
   `TWITCH_CLIENT_ID` / `TWITCH_CLIENT_SECRET`.
2. A one-time grant from the broadcaster (`channel:bot`) and from the user chat
   is read as (`user:read:chat`, `user:bot`; by default that is the
   broadcaster too, otherwise pass `--twitch-as LOGIN`). `chat twitch-auth-url
   --redirect-uri URI` prints the link to open; the redirect URI must be
   registered for the app.
3. A public HTTPS URL on port 443 (reverse proxy, tunnel, ...) that forwards
   to the local webhook listener:

       chat stream --twitch harlybarluy \
         --twitch-callback https://chat.example.org/twitch \
         --twitch-port 8081 --serve 8080

Developed for:

- [twitch/harlybarluy](https://twitch.tv/harlybarluy)
- [youtube/@HarlyBarluy](https://youtube.com/@HarlyBarluy)
