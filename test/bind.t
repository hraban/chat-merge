Simple but effective error messages:

  $ TWITCH_CLIENT_ID=x TWITCH_CLIENT_SECRET=x chat stream --twitch x \
  >   --public-url https://example.invalid/ --serve --port 8099 \
  >   --bind nosuch.invalid
  stream: --bind: cannot resolve "nosuch.invalid"
  [2]

  $ python3 -c 'import socket, time
  > s = socket.socket(); s.bind(("127.0.0.1", 18099)); s.listen()
  > print("up", flush=True); time.sleep(30)' > up &
  $ trap "kill $!" EXIT
  $ while [ ! -s up ]; do sleep 0.05; done
  $ YOUTUBE_API_KEY=x chat stream --youtube x --serve --port 18099
  stream: --port: 127.0.0.1 port 18099 already in use
  [2]
