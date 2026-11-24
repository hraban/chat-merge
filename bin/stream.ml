(* Twitch and YouTube chat interleaved on stdout as messages arrive. Either
   source may be left out. *)

open! Core
open Option.Monad_infix

let run ~serve ~bind ?twitch ?twitch_callback ~twitch_port ?twitch_as ?youtube
    () =
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_unix.use_default () ;
  if Option.is_none twitch && Option.is_none youtube then
    Output.usage_error env "pass --twitch and/or --youtube" ;
  (* Resolved at most once, and only if something listens on it, so a bad
     --bind doesn't fail a run that never uses it. *)
  let addr = lazy (Output.resolve_bind env bind) in
  (* Check for credentials up front, not after output has started. *)
  let twitch =
    twitch
    >>| fun channel ->
    let callback =
      match twitch_callback with
      | Some url ->
          url
      | None ->
          Output.usage_error env "--twitch needs --twitch-callback"
    in
    let client_id, client_secret = Twitch.Client.credentials env in
    let listen = `Tcp (Lazy.force addr, twitch_port) in
    ( { Twitch.Client.client_id
      ; client_secret
      ; callback
      ; listen
      ; read_as= twitch_as }
    , channel )
  in
  let youtube =
    youtube >>| fun target -> (Youtube.Client.api_key env, target)
  in
  Output.with_sink env ~serve ~addr
  @@ fun stream ->
  Eio.Fiber.both
    (fun () ->
      Option.iter twitch ~f:(fun (config, channel) ->
          Twitch.Client.follow env ~config ~stream [channel] ) )
    (fun () ->
      Option.iter youtube ~f:(fun (key, target) ->
          Youtube.Client.follow_targets env ~key ~stream [target] ) )
