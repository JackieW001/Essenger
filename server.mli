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

type sender = string 
type recipient = string 
type timestamp = string
type message = string

(** [auth u] returns true if [u] is a registered username*)
val auth : sender -> bool 

(** [add_msg s r m] adds a message [m] to the database under the conversation
    between sender [s] and recipient [r]*)
val add_msg: sender -> recipient -> message -> bool 

(** [convert_time t] converts time from GMT to specified timezone [t]. 
    Requires: timezones are in their capitalized abreviations. 
    E.g. EST, GMT, PST *)
val convert_time: string -> timestamp

(** [get_msg s r i] returns the last [i] messages between [s] and [r] as
    a JSON file. *)
val get_msg: sender -> recipient -> int -> Yojson.Basic.t 

