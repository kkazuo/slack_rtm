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

let get_url_from_json json =
  let open Yojson.Safe.Util in
  json
  |> Yojson.Safe.from_string
  |> member "url"
  |> to_option to_string

let get_ws_uri token =
  Cohttp_async.Client.post
    (Uri.of_string "https://slack.com/api/rtm.start")
    ~headers:Cohttp.Header.(init_with
                              "Content-Type"
                              "application/x-www-form-urlencoded")
    ~body:Cohttp_async.Body.(of_string ("token=" ^ token))
  >>= fun (_, body) ->
  Pipe.to_list (Cohttp_async.Body.to_pipe body) >>| fun strings ->
  get_url_from_json (String.concat strings)

let rtm_parse ~type_filter s =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string s in
  match json |> member "type" |> to_string with
  | "pong" ->
    (*json |> member "time" |> to_string
    |> Time.of_string_abs
    |> Time.(diff (now ()))
    |> Time.Span.to_float
    |> printf "%f"*)
    ()
  | "presence_change"
  | "reconnect_url" -> ()
  | _ as t ->
    match type_filter with
    | [] ->
      printf "%s\n" s
    | _ ->
      if List.exists type_filter ~f:(fun x -> x = t)
      then printf "%s\n" s

let ping id w =
  `Assoc [ ("type", `String "ping")
         ; ("id",   `Int id)
         ; ("time", `String
              Time.(to_string_abs (now ()) ~zone:Time.Zone.utc)) ]
  |> Yojson.Basic.to_string
  |> Pipe.write w

let f ~type_filter _sock r w _ssl =
  let id = ref 0 in
  let read_line_and_write_to_pipe w =
    let rec loop () =
      Reader.(read_line Lazy.(force stdin)) >>= function
      | `Eof ->
        Pipe.close w;
        Shutdown.exit 0
      | `Ok s -> Pipe.write w s >>= loop
    in loop () in
  let rec keepalive w =
    Time.Span.(of_sec 5.) |> after >>= fun () ->
    if Pipe.is_closed w
    then Deferred.unit
    else begin
      id := !id + 1;
      ping !id w >>= fun () ->
      keepalive w
    end in
  let rec loop () =
    Pipe.(read r) >>= function
    | `Eof ->
      printf "Got EOF. Closing pipe.";
      (*Pipe.close w;*)
      Shutdown.exit 0
    | `Ok s ->
      rtm_parse s ~type_filter;
      return () >>= loop in
  don't_wait_for @@ read_line_and_write_to_pipe w;
  don't_wait_for @@ keepalive w;
  loop ()

let rtm_connect token types =
  (match token with
  | None -> Sys.getenv "SLACK_API_TOKEN"
  | Some _ -> token)
  |> function
  | None ->
    printf "No -token flag nor SLACK_API_TOKEN env found.";
    return ()
  | Some token ->
    get_ws_uri token
  >>= function
  | Some uri ->
    Ws.ws_client uri ~f:(f ~type_filter:types)
  | None ->
    printf "Fail rtm.start";
    return ()
