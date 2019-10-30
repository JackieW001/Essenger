type message = string

type username = string

type command = 
  | Get of username 
  | Send of username * message
  | Help
  | Logout

exception Empty

exception Malformed

(** [string_list_to_string lst] returns a concatenated string s from the
    string list lst from left to right. *)
let rec string_list_to_string (lst:string list) = 
  let ex_name = 
    match lst with
    | [] -> ""
    | h :: t -> h ^ " " ^ (string_list_to_string t) in
  String.trim ex_name

let parse input = 
  let input_string_list = String.split_on_char ' ' (String.trim input) in
  if List.length input_string_list = 0 then raise Empty
  else
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
      if List.length mess > 0 
      then 
        Send (comm,string_list_to_string mess)
      else
        Get comm
    else raise Malformed

