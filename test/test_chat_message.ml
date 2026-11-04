open! Core
open Twitch_chat

let print_twitch line =
  match Irc.parse line with
  | None -> print_endline "(unparsable)"
  | Some msg -> (
      match Twitch.message_of_irc msg with
      | None -> print_endline "(no message)"
      | Some m -> print_endline (Chat_message.to_line m))

let%expect_test "twitch: plain message uses display-name" =
  print_twitch "@display-name=Foo :foo!foo@foo.tmi.twitch.tv PRIVMSG #chan :hi there";
  [%expect {| #chan <Foo> hi there |}]

let%expect_test "twitch: empty display-name falls back to nick" =
  print_twitch "@display-name= :foo!foo@foo.tmi.twitch.tv PRIVMSG #chan :hi";
  [%expect {| #chan <foo> hi |}]

let%expect_test "twitch: /me becomes an action" =
  print_twitch ":foo!foo@foo.tmi.twitch.tv PRIVMSG #chan :\001ACTION waves\001";
  [%expect {| #chan * foo waves |}]

let%expect_test "twitch: non-chat lines are ignored" =
  print_twitch ":tmi.twitch.tv 001 justinfan123 :Welcome, GLHF!";
  [%expect {| (no message) |}]

let%expect_test "to_line sanitizes chat text" =
  print_endline
    (Chat_message.to_line
       {
         platform = Youtube;
         channel = "Some Channel";
         author = "mallory";
         text = "\027[2Jgotcha\xe2\x80\xae";
         kind = Say;
       });
  [%expect {| Some Channel <mallory> ⟨U+001B⟩[2Jgotcha⟨U+202E⟩ |}]
