open Yojson
open Lwt
open Cohttp
open Cohttp_lwt_unix
(** This will be the file representing the server *)
type sender = string 
type recipient = string 
type timestamp = string
type message = string

let proj_id = "essenger-61fdc"
let firebase = "https://"^proj_id^".firebaseio.com/"

let auth sender = 
  failwith "u"

let add_msg sender recipient msg = 
  failwith "u"

let convert_time timezone = 
  failwith "u"

let get_msg sender recipient i = 
  failwith "u"

let retrieve =
  Client.get (Uri.of_string (firebase^"/User/bob.json")) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let put_in_data = 
  let data = Cohttp_lwt.Body.of_string "{\"City\":\"of stars\"}" in 
  Client.post ~body:data (Uri.of_string (firebase^"/User/.json")) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let ()=
  let get = Lwt_main.run retrieve in
  print_endline ("Received body\n" ^ get);
  let put = Lwt_main.run put_in_data in 
  print_endline ("Received body\n" ^ put);
