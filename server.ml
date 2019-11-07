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

(******** USER FUNCTIONS ***********)

(** [retrieve_user] retrieves data associated with user *)
let retrieve_user user =
  Client.get (Uri.of_string (firebase^"/Users/"^user^".json")) 
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

(** [create_user] creates user with associated password [pass]. 
    Currently private function to be implemented later *)
let create_user user pass = 
  let data = Cohttp_lwt.Body.of_string ("{\"password\":\""^pass^"\"}") in 
  Client.put ~body:data (Uri.of_string (firebase^"/Users/"^user^".json"))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let auth sender = 
  failwith "u"

let convert_time timezone = 
  failwith "u"

(******** MESSAGE FUNCTIONS ***********)

(** [clean_word] creates a substring of the first alphanumeric character
    to the last alphanumeric character (inclusively)  *)
let clean_word s : string = 
  try
    let regexp = Str.regexp "[A-Za-z0-9]" in
    let start_pos = Str.search_forward regexp s 0 in 
    let end_pos = Str.search_backward regexp s (String.length s) in
    String.lowercase_ascii
      (String.sub s start_pos (1+end_pos-start_pos))
  with _ -> raise Not_found

(** [return_body] returns the body of an http request *)
let return_body request = 
  request >>= fun(resp,body) -> 
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body

(** [get_num_msgs] returns the number of messages in a converstation *)
let get_num_msgs user1 user2 = 
  let num_msg = 
    Client.get 
      (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^"/num_msg/num_msg.json")) 
    |> return_body |> Lwt_main.run in
  num_msg |> clean_word |> int_of_string

(** [inc_num_msgs] increments the number of messages in a conversation by 1 *)
let inc_num_msgs user1 user2 = 
  let num_msg = 
    Client.get 
      (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^"/num_msg/num_msg.json")) 
    |> return_body |> Lwt_main.run in
  let new_num = (num_msg |> clean_word |> int_of_string)+1 |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"num_msg\":\""^new_num^"\"}") in 
  let _ = Client.put ~body: data 
      (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^"/num_msg.json"))
          |> return_body |> Lwt_main.run in 
  ()

let add_msg user1 user2 msg = 
  let next_msg_num = (get_num_msgs user1 user2) + 1 |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"message\":\""^msg^"\"}") in 
  let _ = Client.put 
      ~body:data (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^"/"^next_msg_num^".json"))
          |> Lwt_main.run;
    inc_num_msgs user1 user2 in 
  ()

let get_msg sender recipient i = ()

(** [create_conversation] creates a new conversation *)
let create_conversation user1 user2 =
  let data = Cohttp_lwt.Body.of_string ("{\"message\":\"Fantastic!\"}") in 
  Client.put ~body:data (Uri.of_string (firebase^"/Conversations/"^user1^"_to_"^user2^"/1.json"))
  >>= fun (resp,body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  (*Printf.printf "%s" ((Cohttp_lwt.Body.to_string:(Cohttp_lwt.Body.t->string)) data);*)
  body

(** [delete_conversation] deletes a conversation *)
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

(* Below is used for testing*)

let ()=
  print_newline();
  (*
  print_endline(get_num_msgs "bob" "michael" |> string_of_int);
  add_msg "bob" "michael" "Fantastic!";
  *)
  (*
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
  let created_conversation = Lwt_main.run (create_conversation "bob" "michael") in
  print_endline ("Received body\n" ^ created_conversation);
  let update_msg_num = Lwt_main.run (set_num_msg "bob" "michael" "3") in
  print_endline ("Received body\n" ^ created_conversation);
  *)
(*
  let deleted_user = Lwt_main.run (delete_user "michael") in 
  print_endline ("Received body\n" ^ deleted_user);
  let deleted_conversation = Lwt_main.run (delete_conversation "bob" "michael") in
  print_endline ("Received body\n" ^ deleted_conversation);
*)