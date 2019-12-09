(** 
   Parsing of user inputs.
*)

(** The type [message] represents the message that a user sends to another 
    user. *)
type message = string

(** The type [username] represents the name of a Essenger user. *)
type username = string

(** The type [groupname] represents the name of a GroupChat. *)
type groupname = string

(** The [command] type is a variant for the multiple commands essenger can 
    process.
    [Get user] returns the message history of the current user with [user].
    [Send (user,msg)] sends [message] to [user].
    [Help] returns the help menu.
    [Friends] returns a list of users the current user has conversations with. 
    [Logout] exits Essenger. 
    [Sticker] shows available stickers in Essenger and how to use them.
    [Emojis] shows available emojis in Essenger and how to use them.
    [GroupChat (name, users)] creates a groupchat with [name] with list [users] 
    in that group chat.
    [Tictactoe (user,str)]
*)
type command = 
  | Get of username 
  | Send of username * message
  | Friends 
  | Help
  | Logout 
  | Sticker
  | Emojis
  | GroupChat of groupname * (username list)
  | Tictactoe of username * string
  | GroupChatGet of groupname
  | GroupChatSend of groupname * message
  | GroupChatAdd of groupname * (username list)

(** Raised when an empty command is entered. *)
exception Empty

(** Raised when a malformed command is entered. *)
exception Malformed

(** [parse str] parses user input from Essenger and turns it into a command.
    The first word is the command word that must contain @ to ensure that it is 
    the command. The rest of the string, if it exists, are the arguments for the
    command.
    Raises:
    [Malformed] if the input does not follow the specified 
    syntax of the program.
    [Empty] if the input string is the empty string.
*)
val parse : string -> command 