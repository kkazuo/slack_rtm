(*
Copyright 2016 Kazuo Koga

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
open Core.Std
open Async.Std
open Async_ssl.Std
open Cohttp

let ssl_client f s r w =
  let app_to_ssl, aw = Pipe.create () in
  let ar, ssl_to_app = Pipe.create () in
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  Ssl.client ~version:Ssl.Version.Tlsv1_2
    ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ()
  |> Deferred.Or_error.ok_exn >>= fun c ->
  Reader.of_pipe Info.(of_string "ssl_reader") ar >>= fun r ->
  Writer.of_pipe Info.(of_string "ssl_writer") aw >>= fun (w, _) ->
  f s r w (Some c)

let connect uri f =
  let host = Option.value_exn ~message:"no host" Uri.(host uri) in
  let scheme = Option.value_exn ~message:"no scheme" Uri.(scheme uri) in
  let secure = scheme = "https" || scheme = "wss" in
  let port = Option.value Uri.(port uri)
      ~default:(if secure then 443 else 80) in
  let handle s r w =
    if secure
    then ssl_client f s r w
    else f s r w None in
  Tcp.(with_connection (to_host_and_port host port)) handle

let ws_client uri ~f =
  let uri = Uri.of_string uri in
  connect uri (fun sock r w ssl ->
      Websocket_async.client_ez uri sock r w >>= fun (r, w) ->
      f sock r w ssl)
