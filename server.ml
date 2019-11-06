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

let retrieve_user user =
  Client.get (Uri.of_string (firebase^"/Users/"^user^".json")) 
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let create_user user pass = 
  let data = Cohttp_lwt.Body.of_string ("{\"password\":\""^pass^"\"}") in 
  Client.post ~body:data (Uri.of_string (firebase^"/Users/"^user^".json"))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let delete_user user = 
  Client.delete (Uri.of_string (firebase^"/Users/"^user^".json"))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  (*Printf.printf "%s" ((Cohttp_lwt.Body.to_string:(Cohttp_lwt.Body.t->string)) data);*)
  body

let create_conversation user1 user2 =
  let data = Cohttp_lwt.Body.of_string ("{\""^user1^"_to_"^user2^"\":\""^user1^"_to_"^user2^".json\"}") in 
  Client.post ~body:data (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^".json"))
  >>= fun (resp,body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  (*Printf.printf "%s" ((Cohttp_lwt.Body.to_string:(Cohttp_lwt.Body.t->string)) data);*)
  body

let delete_conversation user1 user2 = 
  Client.delete (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^".json"))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  (*Printf.printf "%s" ((Cohttp_lwt.Body.to_string:(Cohttp_lwt.Body.t->string)) data);*)
  body

let ()=
  let retrieved_user_1 = Lwt_main.run (retrieve_user "bob") in
  print_endline ("Received body\n" ^ retrieved_user_1);
  print_newline();
  let retrieved_user_2 = Lwt_main.run (retrieve_user "mike") in
  print_endline ("Received body\n" ^ retrieved_user_2);
  print_newline();
  let created_user = Lwt_main.run (create_user "michael" "clarkson") in 
  print_endline ("Received body\n" ^ created_user);
  print_newline();
  let created_conversation = Lwt_main.run (create_conversation "bob" "michael") in
  print_endline ("Received body\n" ^ created_conversation);
  (*
  let deleted_user = Lwt_main.run (delete_user "michael") in 
  print_endline ("Received body\n" ^ deleted_user);
  let deleted_conversation = Lwt_main.run (delete_conversation "bob" "michael") in
  print_endline ("Received body\n" ^ deleted_conversation);
  *)