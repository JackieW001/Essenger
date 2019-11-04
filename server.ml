open Yojson
open Lwt
open Cohttp
open Cohttp_lwt_unix
(** This will be the file representing the server *)
type sender = string 
type recipient = string 
type timestamp = string
type message = string

let auth sender = 
  failwith "u"

let add_msg sender recipient msg = 
  failwith "u"

let convert_time timezone = 
  failwith "u"

let get_msg sender recipient i = 
  failwith "u"

let body =
  Client.get (Uri.of_string "https://www.google.com/") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let ()=
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body);
  print_endline ("end reddit page")
