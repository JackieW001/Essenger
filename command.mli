(** 
   Parsing of user inputs.
*)

(** The type [message] represents the message that a user sends to another 
    user. *)
type message

type command

exception Empty

exception Malformed

val parse : string -> command 