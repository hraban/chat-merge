(* Twitch and YouTube chat interleaved on stdout, or on a web page, as
   messages arrive. Either source may be left out. *)

open! Core
open Chat_merge

let ( let+ ) = Option.( >>| )

let run ~serve ~port ~bind ?twitch ?public_url ?twitch_as ?youtube () =
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_unix.use_default () ;
  if Option.is_none twitch && Option.is_none youtube then
    Usage.error env "pass --twitch and/or --youtube" ;
  let stream = Eio.Stream.create 1024 in
  (* Check for credentials up front, not after output has started. *)
  let twitch =
    let+ channel = twitch in
    let public_url =
      match public_url with
      | Some url ->
          url
      | None ->
          Usage.error env "--twitch needs --public-url"
    in
    let client_id, client_secret = Twitch.Client.credentials env in
    ( Twitch.Client.create
        { client_id
        ; client_secret
        ; callback= Twitch.Client.callback ~public_url
        ; read_as= twitch_as }
        ~on_message:(Eio.Stream.add stream)
    , channel )
  in
  let youtube =
    let+ target = youtube in
    (Youtube.Client.api_key env, target)
  in
  let web = Option.some_if serve (Web_server.create ()) in
  let routes =
    Option.to_list
      (let+ client, _ = twitch in
       (Twitch.Client.webhook_path, Twitch.Client.webhook_handler env client) )
  in
  let default =
    Option.value_map web ~default:Web_server.not_found ~f:(fun web ->
        Web_server.handler web env )
  in
  let produce () =
    Eio.Fiber.both
      (fun () ->
        Option.iter twitch ~f:(fun (client, channel) ->
            Twitch.Client.follow env client [channel] ) )
      (fun () ->
        Option.iter youtube ~f:(fun (key, target) ->
            Youtube.Client.follow_targets env ~key ~stream [target] ) )
  in
  let output () = Output.with_sink env ~web ~stream produce in
  match (web, routes) with
  | None, [] ->
      (* Nothing to listen for, so a bad --bind doesn't fail the run. *)
      output ()
  | _ ->
      Eio.Switch.run
      @@ fun sw ->
      (* Listening before anything subscribes: Twitch posts the webhook
         verification challenge right away. *)
      let socket =
        Output.listen env ~sw ~addr:(Output.resolve_bind env bind) ~port
      in
      Eio.Fiber.first
        (fun () -> Web_server.serve ~socket ~routes ~default)
        output
