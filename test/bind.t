Simple but effective error messages:

  $ TWITCH_CLIENT_ID=x TWITCH_CLIENT_SECRET=x chat stream --twitch x \
  >   --twitch-callback https://example.invalid/ --serve 8099 \
  >   --bind nosuch.invalid 2>&1 | head -4
  stream: --bind: cannot resolve "nosuch.invalid"
