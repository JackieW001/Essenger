(** 
   Parsing of user inputs.
*)

(** The type [message] represents the message that a user sends to another 
    user. *)
type message = string

(** the type [username] represents the name of a Essenger user. *)
type username = string

(** The [command] type is a variant for the multiple commands essenger can 
    process.
    [Get user] returns the message history of the current user with [user].
    [Send (user,msg)] sends [message] to [user].
    [Help] returns the help menu.
    [Friends] returns a list of users the current user has conversations with. 
    [Logout] exits Essenger. 
    [GroupChat (name, users)] creates a groupchat with [name] with list [users] 
    in that group chat.

    Future Commands:
    [Multi]
    [Find] find users with a username/real name
*)
type command = 
  | Get of username 
  | Send of username * message
  | Friends 
  | Help
  | Logout 
  | Sticker
  | Emojis
  | GroupChat of string * (username list)
  | GroupChatGet of string
  | GroupChatSend of string * message
  | GroupChatAdd of string * (username list)

(** Raised when an empty command is entered. *)
exception Empty

(** Raised when a malformed command is entered. *)
exception Malformed

(* 
(** Raised when an unknown username is entered. *)
exception UnknownUser 
*)

(** [parse str] parses user input from Essenger and turns it into a command.
    The first word is the command word that must contain @ to ensure that it is 
    the command. The rest of the string, if it exists, is the message 
    to be sent.
    Raises:
    [Malformed] if the input does not follow the specified 
    syntax of the program.
    [Empty] if the input string is the empty string.
    [UnknownUser] if the username extracted from the input is not a valid 
    username.
*)
val parse : string -> command 