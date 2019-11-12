open Yojson.Basic
open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Sha256
(** This will be the file representing the server *)
type sender = string 
type recipient = string 
type timestamp = string
type message = string

type user_info = 
  {
    password : string;
  }

type conv_info = 
  {
    num_msgs : int;
    conversation : (sender * recipient * message) list;
  }

let proj_id = "essenger-61fdc"
let firebase = "https://"^proj_id^".firebaseio.com/"

(*********** I/O FUNCTIONS *****************)

(** [userjson_to_list j] returns a record representation of the json that 
    represents a user. [j] must be a string json representation that represents
    a user in Essenger. *)
let userjson_to_record j =
  let json = from_string j in
  {
    password = json |> member "password" |> to_string;
  }

let build_conv_list j = 
  let num_msgs = j |> member "num_msg" |> member "num_msg" |> to_string
                 |> int_of_string  in 
  let acc = ref [] in
  for i = 1 to num_msgs do 
    print_endline ("starting num: "^(string_of_int i));
    acc := ((j |> member (string_of_int i) |> member "sender" |> to_string),
            (j |> member (string_of_int i)|> member "recipient" |> to_string),
            (j |> member (string_of_int i) |> member "message" |> to_string)) 
           :: !acc;
  done;
  !acc

(** [convjson_to_record j] returns a record representation of the json that 
    represents a conversation. [j] must be a string json representation that 
    represents a conversation in Essenger. *)
let convjson_to_record j = 
  let json = from_string (j |> Lwt_main.run) in 
  {
    num_msgs = json |> member "num_msg" |> member "num_msg" |> to_string 
               |> int_of_string;
    conversation = build_conv_list json;
  }

(******** USER FUNCTIONS ***********)

let retrieve_user user =
  Client.get  (Uri.of_string (firebase^"/Users/"^user^".json")) 
  >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let create_user user pass = 
  let data = Cohttp_lwt.Body.of_string ("{\"password\":\""^pass^"\"}") in 
  Client.put ~body:data (Uri.of_string (firebase^"/Users/"^user^".json"));
  ()

(** [substring contains s1 s2] returns true if s2 is a substring in s1. *)
let substring_contains s1 s2 = 
  let regexp = Str.regexp_string s2 in
  try ignore (Str.search_forward regexp s1 0); true
  with Not_found -> false

let user_exists user : bool = 
  let (body_string:string) = retrieve_user user |> Lwt_main.run in 
  not (substring_contains body_string "null")

let auth user pass = 
  try
    let user_info = retrieve_user user |> Lwt_main.run in
    let init_ctx = init () in 
    update_string init_ctx pass; 
    let hashed_password = to_hex (finalize init_ctx) in
    (userjson_to_record user_info).password = hashed_password 
  with
  | Yojson.Basic.Util.Type_error (a,b) -> false

let convert_time timezone = 
  failwith "u"

(******** MESSAGE FUNCTIONS ***********)

(** [sort_users] takes in two users and returns tuple of users from smallest
    to largest *)
let sort_users user1 user2 =
  if (user1 > user2) then (user2,user1) else (user1,user2)

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

(** [print_body] prints and returns the body of an http request *)
let print_body request = 
  request >>= fun(resp,body) -> 
  body |> Cohttp_lwt.Body.to_string >|= fun body -> print_endline body; body

(** [get_num_msgs] returns the number of messages in a converstation *)
let get_num_msgs user1 user2 = 
  let users = sort_users user1 user2 in 
  let num_msg = 
    Client.get 
      (Uri.of_string (firebase^"/Conversations/"^(fst users)^
                      "_to_"^(snd users)^"/num_msg/num_msg.json")) 
    |> return_body |> Lwt_main.run in
  if (substring_contains num_msg "null") then 0 else 
    num_msg |> clean_word |> int_of_string

(** [inc_num_msgs] increments the number of messages in a conversation by 1 *)
let inc_num_msgs user1 user2 =
  let users = sort_users user1 user2 in  
  let new_num = ((get_num_msgs user1 user2)+ 1) |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"num_msg\":\""^new_num^"\"}") in 
  Client.put ~body: data 
    (Uri.of_string (firebase^"/Conversations/"^(fst users)^
                    "_to_"^(snd users)^"/num_msg.json"));
  ()

let add_msg user1 user2 msg =
  let users = sort_users user1 user2 in 
  let next_msg_num = (get_num_msgs user1 user2) + 1 |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"sender\":\""^user1^
                                        "\",\"recipient\":\""^user2^
                                        "\",\"message\":\""^msg^"\"}") in 
  let _ = Client.put ~body:data 
      (Uri.of_string (firebase^"/Conversations/"^(fst users)^
                      "_to_"^(snd users)^"/"^next_msg_num^".json"))
          |> return_body |> Lwt_main.run in 
  inc_num_msgs user1 user2; 
  ()

(* This code is unneccessary, add_msg does the same but more
   let create_conversation user1 user2 msg =
   let users = sort_users user1 user2 in
   let num_data = Cohttp_lwt.Body.of_string("{\"num_msg\":\"1\"}") in 
   Client.put ~body:num_data (Uri.of_string (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^"/num_msg.json"));
   let data = Cohttp_lwt.Body.of_string ("{\"sender\":\""^user1^
                                        "\",\"recipient\":\""^user2^
                                        "\",\"message\":\""^msg^"\"}") in
   Client.put ~body:data (Uri.of_string (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^"/1.json"));
   ()
*)

let get_conversation user1 user2 = 
  let users = sort_users user1 user2 in
  Client.get (Uri.of_string (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^".json"))
  >>= fun(resp,body) -> 
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body 

let delete_conversation user1 user2 = 
  let users = sort_users user1 user2 in 
  Client.delete (Uri.of_string (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^".json"));
  ()

let conversation_exists user1 user2 =
  let (body_string:string) = get_conversation user1 user2 |> Lwt_main.run in 
  not (substring_contains body_string "null")

let get_msg user1 user2 i = 
  let users = sort_users user1 user2 in 
  let data = 
    Client.get 
      (Uri.of_string (firebase^"/Conversations/"^(fst users)^
                      "_to_"^(snd users)^"/"^(string_of_int i)^".json")) 
    |> return_body |> Lwt_main.run in
  if (substring_contains data "null") then failwith "message not found" else 
    data

(* Below is used for testing *)

let ()= ()
(* TESTING ADDING NEW MESSAGES TO FIREBASE *)
(* print_endline(get_num_msgs "bob" "michael" |> string_of_int); *)
(* get_msg "jackie" "banpreet" 1; *)
(*  TESTING DELETING A USER
    let deleted_user = Lwt_main.run (delete_user "michael") in 
    print_endline ("Received body\n" ^ deleted_user);
    let deleted_conversation = Lwt_main.run (delete_conversation "bob" "michael") in
    print_endline ("Received body\n" ^ deleted_conversation);
*)