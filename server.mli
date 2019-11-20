(** 
   Server that stores csv files of two user's converstation history. 
   Csv files are named <user1>_<user2>.csv and stores the conversation
   history between <user1> and <user2>.
   Each rows of the csv will store:
    1. the timestamp of when the message was sent as a string
    2. the sender of the message
    3. the message as a string

   The server will be able to: 
   - authenticate user login 
   - parse a json file for the timestamp, sender, recipient, and message
   - create a new csv file between two autheticated users if a conversation
      was just started
   - append new rows (aka messages) to an existing csv file
   - send the last five messages from a sender to a recipient
*)

(** [sender] represents the username of the sender of a message. *)
type sender = string 

(** [recipient] represents the username of the recipient of a message. *)
type recipient = string 

(** [timestamp] represents the time a message was sent. *)
type timestamp = string

(** [message] represents the message that is to be sent. *)
type message = string

(** [auth u] returns true if [u] is a registered username and [p] is the 
    correct corresponding password. *)
val auth : sender -> string -> bool 

(** [convert_time t] converts time from GMT to specified timezone [t]. 
    Requires: timezones are in their capitalized abreviations. 
    E.g. EST, GMT, PST *)
val convert_time: string -> timestamp

(** [create_user] creates user with associated password [pass]. 
    Currently private function to be implemented later *)
val create_user: string -> string -> string Lwt.t

(** [retrieve_user] retrieves data associated with user *)
val retrieve_user: string -> string Lwt.t

(** [user_exists user] returns true if [user] is has a username and password
    in Essenger, false otherwise. *)
val user_exists: string -> bool

(**********************Conversation Functions***********************)

(** [add_msg s r m] adds a message [m] to the database under the conversation
    between sender [s] and recipient [r]. Creates a new conversation between 
    [s] and [r] if the conversation does not exist. *)
val add_msg: sender -> recipient -> message -> unit

(** [get_msg s r i] prints the last [i] messages between [s] and [r] as
    a JSON file. *)
val get_conversation_history: sender -> recipient -> int -> unit

(** [get_conversation u1 u2] returns a json string representation of an existing
    conversation between [u1] and [u2]. Returns "null" if the conversation does
    not exist. *)
val get_conversation : string -> string -> string Lwt.t

(** [delete_conversation u1 u2] deletes a conversation between [u1] and [u2].
    Does nothing if conversation does not exist.*)
val delete_conversation : string -> string -> unit

(** [conversation_exists u1 u2] returns true if [u1] and [u2] have a 
    conversation in the database, false otherwise. *)
val conversation_exists : string -> string -> bool

