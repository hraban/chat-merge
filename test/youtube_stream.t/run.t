Following YouTube chat over liveChatMessages.stream, against a fake API server
(fake_yt.py) that streams its responses in chunks.

  $ python3 fake_yt.py > port 2>> requests &
  $ pid="$!"
  $ trap "kill $pid" EXIT
  $ while [ ! -s port ]; do sleep 0.05; done
  $ export YOUTUBE_API_BASE_URL=http://127.0.0.1:$(cat port)/
  $ export YOUTUBE_API_KEY=dummy

Each scenario prints chat (stdout), then diagnostics (stderr), then the
requests the server saw.

  $ follow() {
  >   chat stream --youtube "$1" 2> stderr
  >   echo "exit $?"
  >   cat stderr
  >   echo "-- requests"
  >   cat requests
  >   : > requests
  > }

The server closes the first connection cleanly. The client reconnects from the
last page token and stops at the page with offlineAt.

  $ follow ok
  Y <alice> hi
  Y <bob> a "quoted" } brace
  Y <carol> third
  Y <dave> after reconnect
  exit 0
  +chan-ok: stream ended
  -- requests
  GET /videos id=ok part=snippet,liveStreamingDetails
  GET /liveChat/messages/stream liveChatId=chat-ok part=snippet,authorDetails
  GET /liveChat/messages/stream liveChatId=chat-ok pageToken=t1 part=snippet,authorDetails

The first connection is cut mid-array. The client reconnects from the last
page token.

  $ follow cut
  Y <eve> before cut
  Y <frank> resumed
  exit 0
  +chan-cut: End_of_file, reconnecting
  +chan-cut: stream ended
  -- requests
  GET /videos id=cut part=snippet,liveStreamingDetails
  GET /liveChat/messages/stream liveChatId=chat-cut part=snippet,authorDetails
  GET /liveChat/messages/stream liveChatId=chat-cut pageToken=c1 part=snippet,authorDetails

An error element mid-stream ends the stream without a reconnect.

  $ follow midErr
  Y <gina> before error
  exit 0
  +midErr: API error 403 liveChatEnded: The live chat is no longer live.
  -- requests
  GET /videos id=midErr part=snippet,liveStreamingDetails
  GET /liveChat/messages/stream liveChatId=chat-midErr part=snippet,authorDetails

A non-200 response with an array-wrapped error body.

  $ follow hdrErr
  exit 0
  +hdrErr: API error 403 forbidden: header error
  -- requests
  GET /videos id=hdrErr part=snippet,liveStreamingDetails
  GET /liveChat/messages/stream liveChatId=chat-hdrErr part=snippet,authorDetails
