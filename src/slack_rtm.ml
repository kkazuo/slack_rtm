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

let () =
  Command.async
    ~summary:"Receive Slack RTM Messages."
    Command.Spec.(
      empty
      +> flag "-token" (optional string) ~doc:"string Slack API Token"
      +> flag "-type" (listed string) ~doc:"string Show only selected type"
    )
    (fun token types () -> Rtm.rtm_connect token types)
  |> Command.run
    ~version:"0.2.2"
