Simple but effective error messages:

  $ TWITCH_CLIENT_ID=x TWITCH_CLIENT_SECRET=x chat stream --twitch x \
  >   --twitch-callback https://example.invalid/ --serve 8099 \
  >   --bind nosuch.invalid 2>&1 | head -4
  Uncaught exception:
    
    Eio.Io Net Address_lookup_failed NONAME (name or service is not known),
    looking up "nosuch.invalid"
