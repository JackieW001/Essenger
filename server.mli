(** [sender] represents the username of the sender of a message. *)
type sender = string 

(** [recipient] represents the username of the recipient of a message. *)
type recipient = string 

(** [timestamp] represents the time a message was sent. *)
type timestamp = string

(** [message] represents the message that is to be sent. *)
type message = string

(********************** User Functions ***********************)

(** [auth u] returns true if [u] is a registered username and [p] is the 
    correct corresponding password. *)
val auth : sender -> string -> bool 

(** [create_user user] creates [user] with associated password [pass]. 
    Currently private function to be implemented later *)
val create_user: string -> string -> unit

(** [retrieve_user user] retrieves data associated with [user] *)
val retrieve_user: string -> string Lwt.t

(** [user_exists user] returns true if [user] is has a username and password
    in Essenger, false otherwise. *)
val user_exists: string -> bool

(** [delete_user] deletes [user] from database. *)
val delete_user: string -> unit

(********************** Friend Functions ***********************)
(** [get_friends u] returns a list of the friends of [u] *)
val get_friends: string -> string list 

(********************** Message Functions ***********************)

(** [add_msg s r m] adds a message [m] to the database under the conversation
    between sender [s] and recipient [r]. Creates a new conversation between 
    [s] and [r] if the conversation does not exist. *)
val add_msg: sender -> recipient -> message -> unit

(** [get_conversation_history s r i] returns a string list of conversations
    between s and r from oldest to newest *)
val get_conversation_history: sender -> recipient -> string list 

(** [get_conversation u1 u2] returns a json string representation of an existing
    conversation between [u1] and [u2]. Returns "null" if the conversation does
    not exist. *)
val get_conversation : string -> string -> string Lwt.t

(** [conversation_exists u1 u2] returns true if [u1] and [u2] have a 
    conversation in the database, false otherwise. *)
val conversation_exists : string -> string -> bool

(** [delete_conversation u1 u2] deletes a conversation between [u1] and [u2].
    Does nothing if conversation does not exist.*)
val delete_conversation : string -> string -> unit

(********************** Group Chat Functions ***********************)

(** [gc_exists g] returns if group chat named [g] exists. *)
val gc_exists: string -> bool 

(** [create_gc g u] creates a group chat named [g] with users [u]. *)
val create_gc : string -> string list -> unit  

(** [get_gc_users g] returns a list of the users in group chat [g]. 
    Returns:
    [] if group chat doesn't exist  *)
val get_gc_users : string -> string list 

(** [add_gc_msg g u m] adds a message [m] sent by [u] to group chat [g]  *)
val add_gc_msg : string -> string -> string -> unit 

(** [get_gc_history g i ] returns a list of the messages in group chat [g]
    from oldest to newest *)
val get_gc_history : string -> string list 

