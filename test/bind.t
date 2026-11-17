An unresolvable --bind address is a usage error: a one-line message on
stderr and exit status 2.

  $ chat stream --twitch x --serve 8099 --bind nosuch.invalid
  stream: --bind: cannot resolve "nosuch.invalid"
  [2]
