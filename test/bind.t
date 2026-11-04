An unresolvable --bind address is an error.

  $ chat stream --twitch x --serve 8099 --bind nosuch.invalid 2>&1 | head -4
  Uncaught exception:
    
    Eio.Io Net Address_lookup_failed NONAME (name or service is not known),
    looking up "nosuch.invalid"
