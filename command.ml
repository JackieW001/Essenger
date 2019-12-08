type message = string

type username = string

type command = 
  | Get of username 
  | Send of username * message
  | Friends
  | Help
  | Logout
  | Sticker
  | Emojis
  | GroupChat of string * (username list)
  | Tictactoe of username * string

exception Empty

exception Malformed

(*
exception UnknownUser
*)
let stickers = [
  (1, {|(O-O)|}); 
  (2, {|(\^o^/)|});
  (3, {|( .o.)|});
  (4, {|( .-. )|});
  (5, {|( -.- )|})
]

let emojis = [
  ("happy", "\u{1F600}");
  ("sad", "\u{1F614}");
  ("wink", "\u{1F609}");
  ("thinking", "\u{1F914}");
  ("kiss", "\u{1F618}");
  ("heart_eyes","\u{1F60D}");
  ("laughing", "\u{1F602}");
  ("hundred", "\u{1F4AF}");
  ("bang", "\u{1F4A5}");
  ("sleep", "\u{1F4A4}");
  ("heart", "\u{1F493}");
  ("skull", "\u{1F480}");  
  ("monkey", "\u{1F435}");
  ("dog", "\u{1F436}");
  ("cat", "\u{1F431}");
  ("unicorn", "\u{1F984}");
  ("pig", "\u{1F437}")
]

(** [get_emoji id] finds the requested emoji by searching the emoji list for the 
    emoji name [id] that the user inputted. *)
let rec get_emoji id = function
  | [] -> "[emoji]"
  | (i, e)::t -> if (id=i) then e else get_emoji id t

(** [get_sticker i] finds the sticker by searching the list for the number that
    the user inputted.  *)
let rec get_sticker i = function
  | [] -> "[sticker]" (** sticker not found *)
  | (id, s)::t -> if (id = i) then s else get_sticker i t

(** [string_list_to_string lst] returns a concatenated string s from the
    string list lst from left to right. *)
let rec string_list_to_string (lst:string list) = 
  let ex_name = 
    match lst with
    | [] -> ""
    | h :: t -> (
        if (Str.string_match (Str.regexp "\\(^#[a-z]*$\\)") h 0) then 
          (get_emoji (String.sub h 1 (String.length h - 1)) emojis) ^ "  " ^ 
          (string_list_to_string t)
        else if (Str.string_match (Str.regexp "\\(^#[0-9]*$\\)") h 0) then
          get_sticker (int_of_string (String.sub h 1 1)) stickers
        else h ^ " " ^ (string_list_to_string t) ) in
  String.trim ex_name

let parse input = 
  if String.length input = 0 then raise Empty
  else
    let input_string_list = String.split_on_char ' ' (String.trim input) in
    let op = input_string_list |> List.hd in
    let mess = input_string_list |> List.tl in 
    let comm = String.sub op 1 (String.length op - 1) in
    if String.get op 0 = '@' && String.length comm > 0 then
      if comm = "logout" || 
         comm = "Logout" then Logout 
      else 
      if comm = "help" ||
         comm = "Help" then Help
      else
      if comm = "friends" || 
         comm = "Friends" then Friends
      else 
      if comm = "stickers" ||
         comm = "Stickers" then Sticker
      else
      if comm = "emojis" ||
         comm = "Emojis" then Emojis
      else
      if comm = "gc" then GroupChat ((mess |> List.hd) ,(mess |> List.tl))
      else
      if comm = "tictactoe" || comm = "TicTacToe" || comm = "Tictactoe" then (
        try let newgame = (mess |> List.rev |> List.hd) in
          if newgame = "new" then Tictactoe ((mess |> List.hd), "new") 
          else Tictactoe ((mess |> List.hd), "")
        with 
        | hd -> Tictactoe ((mess |> List.hd), "")
      )
      else
      if List.length mess > 0 
      then 
        Send (comm,string_list_to_string mess)
      else
        Get comm
    else raise Malformed

