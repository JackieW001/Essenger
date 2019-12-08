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
    friends: string list; 
    password : string;
  }

type conv_info = 
  {
    num_msgs : int;
    conversation : (sender * recipient * message) list;
  }

type gc_info = 
  {
    conversation: (sender * unit * message) list; 
    num_msgs : int; 
    num_users : int; 
    users: string list; 
  }

let proj_id = "essenger-61fdc"
let firebase = "https://"^proj_id^".firebaseio.com/"

(*********** I/O FUNCTIONS *****************)

(** [get_friend] returns a user *)
let get_user j = 
  j |> member "name" |> to_string

(** [userjson_to_list j] returns a record representation of the json that 
    represents a user. [j] must be a string json representation that represents
    a user in Essenger. *)
let userjson_to_record j =
  let json = from_string j in
  try
    {
      friends = json|> member "friends" |> to_list |> List.map get_user; 
      password = json |> member "password" |> to_string;
    }
  with 
  | Yojson.Basic.Util.Type_error (a,b) -> 
    {
      friends = [];
      password =  json |> member "password" |> to_string;
    }

(** [build_conv_list] adds all conversation data to a list *)
let build_conv_list j = 
  let num_msgs = j |> member "num_msg" |> member "num_msg" |> to_string
                 |> int_of_string  in 
  let acc = ref [] in
  for i = 1 to num_msgs do 
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

(** [build_msg_history] adds a maximum number of [s] converstations to
    a list. *)
let build_msg_history j s = 
  let num_msgs = j |> member "num_msg" |> member "num_msg" |> to_string
                 |> int_of_string  in 
  let acc = ref [] in
  let start = if (num_msgs <= s) then 1 else num_msgs-s in
  for i = start to num_msgs do 
    acc := ((j |> member (string_of_int i) |> member "sender" |> to_string),
            (j |> member (string_of_int i)|> member "recipient" |> to_string),
            (j |> member (string_of_int i) |> member "message" |> to_string)) 
           :: !acc;
  done;
  !acc

(** [histjson_to_record] creates a record of a conversation history *)
let histjson_to_record j = 
  let json = from_string j in 
  {
    num_msgs = json |> member "num_msg" |> member "num_msg" |> to_string 
               |> int_of_string;
    conversation = build_conv_list json ;
  }

(** [build_conv_list] adds all group chat conversation data to a list *)
let build_gc_conv_list j = 
  let num_msgs = j |> member "conversation" |> member "num_msgs" 
                 |> member "num_msgs" |> to_string |> int_of_string  in 
  let acc = ref [] in
  for i = 0 to (num_msgs-1) do 
    acc := ((j |> member "conversation" |> 
             member (string_of_int i) |> member "sender" |> to_string), (),
            (j |> member "conversation" |> 
             member (string_of_int i) |> member "message" |> to_string)) 
           :: !acc;
  done;
  !acc


(** [conv_to_str_list] converts conversation in record to string list *)
let conv_to_str_list (info: conv_info) = 
  let msgs = info.conversation in 
  let rec format_msgs msgs acc = 
    match msgs with 
    | [] -> acc
    | (s,_,m)::t -> format_msgs t ((s ^ ": " ^ m)::acc)
  in 
  (format_msgs msgs []) |> List.rev 

(** [gcjson_to_record] creates a record of a group chat *)
let gcjson_to_record j = 
  let json = from_string j in 
  try 
    {
      conversation = build_gc_conv_list json; 
      num_msgs = json |> member "conversation" |> member "num_msgs" 
                 |> member "num_msgs" |> to_string |> int_of_string; 
      num_users = json |> member "num_users" |> member "num_users" 
                  |> to_string |> int_of_string; 
      users = json|> member "users" |> to_list |> List.map get_user; 
    }
  with 
  | Yojson.Basic.Util.Type_error (a,b) -> 
    {
      conversation = [];
      num_msgs = 0;
      num_users = json |> member "num_users" |> member "num_users" 
                  |> to_string |> int_of_string; 
      users = json|> member "users" |> to_list |> List.map get_user;
    }

(** [gc_conv_to_str_list] converts group chat conversation in record to 
    string list *)
let gc_conv_to_str_list (info: gc_info) = 
  let msgs = info.conversation in 
  let rec format_msgs msgs acc = 
    match msgs with 
    | [] -> acc
    | (s,_,m)::t -> format_msgs t ((s ^ ": " ^ m )::acc)
  in 
  (format_msgs msgs []) |> List.rev

(** [print_list] prints a list where each element is separated by new lines *)
let rec print_list = function 
  | [] -> print_endline ""
  | h::t -> print_endline (h ^ "\n"); print_list t 

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

(******** USER FUNCTIONS ***********)

(** [retrieve_user] returns [user] information from database *)
let retrieve_user user =
  Client.get  (Uri.of_string (firebase^"/Users/"^user^".json")) 
  >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

(** [creates_user] creates [user] and adds password [pass] data *)
let create_user user pass = 
  let data = Cohttp_lwt.Body.of_string ("{\"password\":\""^pass^"\"}") in 
  let _ = Client.put ~body:data (
      Uri.of_string (firebase^"/Users/"^user^".json")) 
          |> return_body |> Lwt_main.run in ()

(** [substring_contains s1 s2] returns true if s2 is a substring in s1. *)
let substring_contains s1 s2 = 
  let regexp = Str.regexp_string s2 in
  try ignore (Str.search_forward regexp s1 0); true
  with Not_found -> false

(** [user_exists] returns true if [user] exists. *)
let user_exists user : bool = 
  let (body_string:string) = retrieve_user user |> Lwt_main.run in 
  not (substring_contains body_string "null")

(** [auth] tries to authenticate [user] with given password [pass].
    Returns: 
    false if authentification is unsuccessful *)
let auth user pass = 
  try
    let user_info = retrieve_user user |> Lwt_main.run in
    let init_ctx = init () in 
    update_string init_ctx pass; 
    let hashed_password = to_hex (finalize init_ctx) in
    (userjson_to_record user_info).password = hashed_password 
  with
  | Yojson.Basic.Util.Type_error (a,b) -> false

(******** FRIEND FUNCTIONS ***********)

(** [get_num_friends] returns number of friends of [user1] *)
let get_num_friends user1 = 
  let num_friends = 
    Client.get 
      (Uri.of_string (firebase^"/Users/"^user1^"/num_friends/num_friends.json")) 
    |> return_body |> Lwt_main.run in
  if (substring_contains num_friends "null") then 0 else 
    num_friends |> clean_word |> int_of_string

(** [inc_num_msgs] increments the number of messages in a conversation by 1 *)
let inc_num_friends user1 =  
  let new_num = ((get_num_friends user1)+ 1) |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"num_friends\":\""^new_num^"\"}") in 
  let _ = Client.put ~body: data 
      (Uri.of_string (firebase^"/Users/"^user1^"/num_friends.json")) 
          |> return_body |> Lwt_main.run in 
  ()

(** [add_friend user1 user2] adds [user2] to friend list of [user1] *)
let add_friend user1 user2 =
  let next_friend_num = (get_num_friends user1)|> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"name\":\""^user2^"\"}") in 
  let _ = Client.put ~body:data 
      (Uri.of_string (firebase^"/Users/"^user1^"/friends/"^next_friend_num^"/.json"))
          |> return_body |> Lwt_main.run in 
  inc_num_friends user1; 
  ()

(** [get_friends] returns the friends of [user1] *)
let get_friends user1 = 
  let json = Client.get 
      (Uri.of_string (firebase^"/Users/"^user1^".json"))
             |> return_body |> Lwt_main.run in 
  let user_info = userjson_to_record json in 
  user_info.friends |> List.sort compare 

let delete_user user1 = 
  let _ = Client.delete 
      (Uri.of_string (firebase^"/Users/"^user1^".json")) in
  ()

(******** NOTIFICATION FUNCTIONS *)

(** [update_notification_from_user] adds [user1], who sent a message to 
    [user2], to [user2]'s list of notifications *)
let update_notification_from_user user1 user2 = 
  let data = Cohttp_lwt.Body.of_string ("{\"blep\": \"\"}") in 
  let _ = Client.put ~body:data 
      (Uri.of_string (firebase^"/Users/"^user2^"/notifications/users/@"
                      ^user1^".json"))
          |> return_body |> Lwt_main.run in
  ()

(** [delete_notification_from_user] deletes notification from [user1] in
    [user2]'s notifications *)
let delete_notification_from_user user1 user2 =
  let _ = Client.delete 
      (Uri.of_string (firebase^"/Users/"^user2^"/notifications/users/@"^user1^
                      ".json")) in
  () 

(** [parse_for_users] cleans string [s] to only have usernames *)
let parse_for_users s = 
  let splitter = Str.regexp("@") in 
  (Str.split splitter s) |> List.tl |> List.map (fun x -> 
      let length = String.index x '\"' in 
      String.sub x 0 length
    )

let get_notifications user = 
  let not = Client.get
      (Uri.of_string (firebase^"/Users/"^user^"/notifications/users/.json"))
            |> return_body |> Lwt_main.run in 
  if (substring_contains not "null") then [] else 
    parse_for_users not

(******** MESSAGE FUNCTIONS ***********)

(** [sort_users] takes in two users and returns tuple of users from smallest
    to largest *)
let sort_users user1 user2 =
  if (user1 > user2) then (user2,user1) else (user1,user2)

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
  let _ = Client.put ~body: data 
      (Uri.of_string (firebase^"/Conversations/"^(fst users)^
                      "_to_"^(snd users)^"/num_msg.json")) 
          |> return_body |> Lwt_main.run in 
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
  update_notification_from_user user1 user2; 
  if next_msg_num = "1" then (add_friend user1 user2;add_friend user2 user1) else 
    ()

(** [get_msg] returns the message of index [i] between [user1] and [user2] *)
let get_msg user1 user2 i = 
  let users = sort_users user1 user2 in 
  let data = 
    Client.get 
      (Uri.of_string (firebase^"/Conversations/"^(fst users)^
                      "_to_"^(snd users)^"/"^(string_of_int i)^".json")) 
    |> return_body |> Lwt_main.run in
  if (substring_contains data "null") then failwith "Message not found" else 
    data

let get_conversation_history user1 user2 = 
  let users = sort_users user1 user2 in
  let request = 
    Client.get 
      (Uri.of_string 
         (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^".json")) 
    |> return_body |> Lwt_main.run in 
  if (substring_contains request "null") then failwith "No message history" else
    let conv_info = histjson_to_record request in 
    delete_notification_from_user user1 user2; 
    conv_to_str_list conv_info


let get_conversation user1 user2 = 
  let users = sort_users user1 user2 in
  Client.get 
    (Uri.of_string 
       (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^".json")) 
  |> return_body 

(** [conversation_exists] returns if a conversation exists between 
    [user1] and [user2] *)
let conversation_exists user1 user2 =
  let (body_string:string) = get_conversation user1 user2 |> Lwt_main.run in 
  not (substring_contains body_string "null")

let delete_conversation user1 user2 = 
  let users = sort_users user1 user2 in 
  let _ = Client.delete 
      (Uri.of_string (firebase^"/Conversations/"^(fst users)^"_to_"^(snd users)^
                      ".json")) in
  ()

(******** GROUP CONVERSATION FUNCTIONS ***********)

(** [string_of_lst_helper] converts list [lst] to a string *)
let rec string_of_lst_helper = function
  | [] -> ""
  | h::t -> let tail = string_of_lst_helper t in
    h ^ (if tail= "" then "" else ", ") ^ tail

(** [string_of_lst] converts list [lst] to a string *)
let rec string_of_lst lst =  
  "["^(string_of_lst_helper lst)^"]"

(** [retrieve_gc] retrieves group chat from database *)
let retrieve_gc gc_name = 
  Client.get (Uri.of_string (firebase^"/GroupChats/"^gc_name^".json")) 
  |> return_body |> Lwt_main.run

let gc_exists gc_name = 
  let (body_string:string) = retrieve_gc gc_name in 
  not (substring_contains body_string "null")

let get_gc_users gc_name = 
  let json = Client.get 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^".json"))
             |> return_body |> Lwt_main.run in 
  if (substring_contains json "null") then [] else 
    let gc_info = gcjson_to_record json in 
    gc_info.users

(** [get_num_users] gets number of users in [gc_name] *)
let get_num_users gc_name =
  let num_users = Client.get 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^"/num_users/num_users.json"))
                  |> return_body |> Lwt_main.run in 
  if (substring_contains num_users "null") then 0 else 
    num_users |> clean_word |> int_of_string

(** [inc_num_users] increments number of users in [gc_name] *)
let inc_num_users gc_name = 
  let new_num = ((get_num_users gc_name)+ 1) |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"num_users\":\""^new_num^"\"}") in 
  let _ = Client.put ~body: data 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^"/num_users.json")) 
          |> return_body |> Lwt_main.run in 
  ()

(** [add_user_to_gc] adds a single user to [gc_name] *)
let add_user_to_gc gc_name user = 
  let num_users = get_num_users gc_name |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"name\":\""^user^"\"}") in 
  let _ = Client.put ~body: data 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^"/users/"^num_users^".json")) in 
  inc_num_users gc_name

let rec add_users_to_gc gc_name user_lst = 
  match user_lst with 
  | [] -> ()
  | h::t -> add_user_to_gc gc_name h; add_users_to_gc gc_name t 

let create_gc gc_name user_lst = 
  add_users_to_gc gc_name user_lst

(** [get_num_gc_msgs] returns the number of messages in a group chat *)
let get_num_gc_msgs gc_name = 
  let num_msg = 
    Client.get 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^"/conversation/"^
                      "num_msgs/num_msgs.json")) 
    |> return_body |> Lwt_main.run in
  if (substring_contains num_msg "null") then 0 else 
    num_msg |> clean_word |> int_of_string

(** [inc_num_gc_msgs] increments the number of messages in a group chat by 1 *)
let inc_num_gc_msgs gc_name =
  let new_num = ((get_num_gc_msgs gc_name)+ 1) |> string_of_int in 
  let data = Cohttp_lwt.Body.of_string ("{\"num_msgs\":\""^new_num^"\"}") in 
  let _ = Client.put ~body: data 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^"/conversation/num_msgs.json")) 
          |> return_body |> Lwt_main.run in 
  ()

let add_gc_msg gc_name user msg =
  let next_msg_num = (get_num_gc_msgs gc_name)|> string_of_int in
  let data = Cohttp_lwt.Body.of_string ("{\"sender\":\""^user^
                                        "\",\"message\":\""^msg^"\"}") in 
  let _ = Client.put ~body:data 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^"/conversation/"^next_msg_num^".json"))
          |> return_body |> Lwt_main.run in 
  inc_num_gc_msgs gc_name; ()

let get_gc_history gc_name = 
  let request = 
    Client.get 
      (Uri.of_string 
         (firebase^"/GroupChats/"^gc_name^".json")) 
    |> return_body |> Lwt_main.run in 
  if (substring_contains request "null") then failwith "No message history" else
    let gc_conv_info = gcjson_to_record request in 
    gc_conv_to_str_list gc_conv_info

let delete_gc gc_name = 
  let _ = Client.delete 
      (Uri.of_string (firebase^"/GroupChats/"^gc_name^".json")) in ()

(* Below is used for testing *)

let ()= (); 
  (* add_msg "test1" "test2" "hi"; 
     add_msg "test1" "test2" "bye"; 
     print_list (get_notifications "test2");
     print_list (get_conversation_history "test1" "test2");*)
  (* print_list (get_notifications "test2"); *) 
  (* update_notification_from_user "test1" "test2"; 
     update_notification_from_user "test3" "test2"; *) 
  (* delete_notification_from_user "test1" "test2"; 
     print_list (get_notifications "test2"); *) 
  (* create_gc "special_surprise" ["jackie";"william"];
     add_gc_msg "special_surprise" "jackie" "bye bye";
     add_gc_msg "special_surprise" "william" "hi"; 
     print_list (get_gc_history "special_surprise"); *) 
  (* create_gc "special_surprise" ["jackie";"william"];
     add_gc_msg "special_surprise" "jackie" "bye bye"; *)

  (* 
  get_gc_users "second chat" |> print_list;
  create_gc "first chat" [];
  *)
  (* create_gc "first chat" ["jackie";"banpreet"]; *)
(*
  print_endline (string_of_lst [""]);
  print_endline (string_of_lst ["hello";"hi"]);
  *) 
  (*
  add_friend "test" "jackie";
    add_friend "test" "banpreet";
    *)
  (*get_friends "test" |> print_list;*)
  (*add_msg "ashneel" "beep" "hello there"; *)
(*
  get_num_friends "ashneel" |> string_of_int |> print_endline;
  add_friend "ashneel" "jackie";
  get_num_friends "ashneel" |> string_of_int |> print_endline;
  add_friend "ashneel" "michelle";
  *)
  (* inc_num_friends "ashneel"; *)
  (*add_friend "ashneel" "jackie";
    get_friends "ashneel";*)
  (* get_conversation_history "jackie" "ashneel" 5; *)
  (* TESTING ADDING NEW MESSAGES TO FIREBASE *)
  (* print_endline(get_num_msgs "bob" "michael" |> string_of_int); *)
  (*inc_num_msgs "jackie" "banpreet" *)
  (*print_endline((get_num_msgs "jackie" "banpreet") |> string_of_int);*)
  (*  TESTING DELETING A USER
      let deleted_user = Lwt_main.run (delete_user "michael") in 
      print_endline ("Received body\n" ^ deleted_user);
      let deleted_conversation = 
      Lwt_main.run (delete_conversation "bob" "michael") in
      print_endline ("Received body\n" ^ deleted_conversation);
  *)