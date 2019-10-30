(** 
   Parsing of user inputs.
*)

(** The type [message] represents the message that a user sends to another 
    user. *)
type message = string

type username = string

type command = 
  | Get of username 
  | Send of username * message
  | Help
  | Logout 

exception Empty

exception Malformed

val parse : string -> command 