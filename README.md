# Chat-merge: display youtube and twitch chat in same session

Mostly AI generated project to merge twitch and youtube live chat sessions into a single session.  Spin up a server, add a web scene in OBS and add it as an overlay to have a diy-multi stream chat overlay.

This project is written in OCaml and heavily uses janestreet core where applicable.

Envvars:

- `YOUTUBE_API_KEY`
- `TWITCH_CLIENT_ID`
- `TWITCH_CLIENT_SECRET`

## Twitch

Twitch chat is read through the Helix API.

Requirements:

1. A Twitch app ("https://dev.twitch.tv"), credentials in env
2. A one-time grant from the broadcaster (`channel:bot`) and from the user chat
   is read as (the broadcaster by default, otherwise pass `--twitch-as MyBotUser`):

       $ chat twitch-auth-url --redirect-uri https://my-bot.example.com

3. A public HTTPS URL on port 443 (reverse proxy, tunnel, ...) that forwards
   to the local webhook listener:

       $ chat stream --twitch harlybarluy \
         --twitch-callback https://my-bot.example.com/twitch \
         --twitch-port 443 --serve 8080

Developed for:

- [twitch/harlybarluy](https://twitch.tv/harlybarluy)
- [youtube/@HarlyBarluy](https://youtube.com/@HarlyBarluy)
