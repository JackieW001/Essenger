open Command
open Server 
open Sha256

(* Helper Functions *)

let stickers = [(1,"(O-O)"); (2, "(\^o^/)")]

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

let rec print_stickers = function
  | [] -> ANSITerminal.(print_string [cyan] "")
  | (id, s)::t -> ANSITerminal.(print_string [white] s); 
    ANSITerminal.(print_string [cyan] (String.concat (string_of_int id) [" "; "\n"]));
    print_stickers t

let rec print_emojis = function
  | [] -> ANSITerminal.(print_string [white] "")
  | (id, e)::t -> ANSITerminal.(print_string [white] e);
    ANSITerminal.(print_string [white] (String.concat id [" "; "\n"]));
    print_emojis t

(** [replace_spaces s] replaces all whitespaces in [s] with '_'. *)
let replace_spaces = Str.global_replace (Str.regexp " ") "_"

(** [print_list l] prints the list [l] *)
let rec print_list = function 
  | [] -> print_endline ""
  | h::t -> print_endline h; print_list t 

(** [main] is the main interface for Essenger. It takes parsed commands from 
    the command module and processes them to perform the proper function as 
    specified by the command. *)
let rec main current_user ()= 
  print_newline();
  ANSITerminal.(print_string [cyan] "\nEssenger\n");
  ANSITerminal.(print_string [white] 
                  "Type @Help or @help to view supported commands.\n");
  print_string "> ";
  try 
    let command = read_line () |> Command.parse in
    match command with
    | Send (r,m) -> (* Send message to server *) 
      if (Server.user_exists r) then (
        ANSITerminal.(print_string [cyan] 
                        ("Recipient: " ^ r ^ "\nMessage: " ^ m));
        Server.add_msg current_user r m;
        main current_user ()
      )
      else (
        ANSITerminal.(print_string [red]
                        ("\nUser \""^r^"\" does not exist."^ 
                         " Check if the username is correct."));
        main current_user ()
      )
    | Get r ->( (* Get message history *) 
        try
          ANSITerminal.(print_string [cyan] 
                          ("Getting message history with: " ^ r ^ "\n"));
          if Server.user_exists r then 
            (Server.get_conversation_history current_user r 5; 
             main current_user ())
          else 
            (ANSITerminal.(print_string [red]
                             ("\nUser " ^ r ^ " does not exist."));
             main current_user ())
        with
        | Failure x -> 
          ANSITerminal.(print_string [red]
                          ("\nYou have no message history with " ^ r));
          main current_user ())
    | Friends -> (* Get List of friends *) 
      ANSITerminal.(print_string [cyan] 
                      ("Getting friends list."));
      print_endline "";
      Server.get_friends current_user |> print_list;
      print_endline "-------------------------------";
      (*ANSITerminal.(print_string [red] 
                      ("\nUnimplemented.")); *)
      main current_user ()
    | Help -> (* Access Help Options *) 
      ANSITerminal.(print_string [cyan]
                      "\nSupported Commands: \n
              @<username> : Gets your message history with <username>\n
              @<username> <message> : Sends <message> to user <username>\n
              @Friends or @friends : View who you've had conversations with.\n
              @gc <gc_name> <username list> : Create a GroupChat with users 
                  specified in <username list>. Every word separated by spaces
                  after <gc_name> specifies a user to add to the GroupChat.\n
              @Logout or @logout : Log out of Essenger.\n");
      main current_user ()
    | Logout -> (* Logout From Account *) 
      ANSITerminal.(print_string [cyan] 
                      "\nLogging Out.\n");
      exit 0
    | Sticker -> 
      ANSITerminal.(print_string [cyan] "\nAvailable stickers:\n");
      print_stickers stickers;
      ANSITerminal.(print_string [cyan] "\nTo send a sticker, enter: \n
      @username #[sticker number].");
      main current_user ()
    | Emojis ->
      ANSITerminal.(print_string [cyan] "\nAvailable emojis: \n");
      print_emojis emojis;
      ANSITerminal.(print_string 
                      [cyan] 
                      "\nTo use an emoji, enter: \n @username #[emoji name].");
      main current_user ()
    | GroupChat (n, ht) -> 
      ANSITerminal.(print_string [cyan] ("\nCreating GroupChat "^n^".\n"));
      if Server.gc_exists n then
        (ANSITerminal.(print_string [red] ("\nGroupChat exists. Please choose "^
                                           "another name. \n"));
         main current_user ())
      else  
        (
          Server.create_gc n (current_user :: ht);
          ANSITerminal.(print_string [green] ("\nGroupChat "^n^" created.\n"));
          main current_user ()
        )

  with
  | Malformed -> print_string 
                   "Please try again. Hint: Did you start with '@'?\n";
    main current_user ()
  | Empty -> print_string "Nothing was inputted, please try again.\n";
    main current_user ()
  (*
  |UnknownUser -> print_string "User not found.\n";
  *)

(** [login] handles username and password input for the client-facing interface.
    [login] takes in user input and passes that information as a JSON to the server,
    which authenticates the login. If login fails, user is prompted to try again.*)
let rec login () = 
  print_endline (" 

 /$$$$$$$$  /$$$$$$  /$$$$$$  /$$$$$$$$ /$$   /$$  /$$$$$$  /$$$$$$$$ /$$$$$$$ 
| $$_____/ /$$__  $$/$$__  $$| $$_____/| $$$ | $$ /$$__  $$| $$_____/| $$__  $$
| $$      | $$  \__/ $$  \__/| $$      | $$$$| $$| $$  \__/| $$      | $$  \ $$
| $$$$$   |  $$$$$$|  $$$$$$ | $$$$$   | $$ $$ $$| $$ /$$$$| $$$$$   | $$$$$$$/
| $$__/    \____  $$\____  $$| $$__/   | $$  $$$$| $$|_  $$| $$__/   | $$__  $$
| $$       /$$   \ $$/$$  \  $$| $$      | $$\   $$$| $$  \  $$| $$      | $$  \\ $$
| $$$$$$$$|  $$$$$$/  $$$$$$/| $$$$$$$$| $$ \   $$| $$$$$$/ | $$$$$$$$| $$  | $$
|________/ \______/ \______/ |________/|__/  \__/ \______/ |________/|__/  |__/

                                                                        ");
  ANSITerminal.(print_string [cyan] 
                  "\nWelcome to Essenger, the Better Messenger.\n");
  ANSITerminal.(print_string [cyan] 
                  "\nAre you a returning user? [y/n/quit]");
  let response = String.trim (read_line ()) in
  if response = "quit" then 
    (ANSITerminal.(erase Screen);exit 0)
  else 
  if response = "y" then (
    print_string "\nEnter your username: ";
    let username_input = replace_spaces (String.trim(read_line ())) in
    print_string "Enter your password: ";
    let password_input = String.trim(read_line ()) in
    if Server.auth username_input password_input then 
      (ANSITerminal.(erase Screen);
       ANSITerminal.(print_string [green] 
                       ("\n Login Successful."));
       main username_input ())
    else
      ANSITerminal.(print_string [red] "\n Incorrect login, try again.");
    login ())
  else(
    if response = "n" then
      (ANSITerminal.(print_string [cyan] 
                       "\nPlease enter a username: ");
       let created_username = replace_spaces(String.trim (read_line ())) in
       if Server.user_exists created_username then
         (ANSITerminal.(print_string [red]
                          "\nUsername already exists. Please try again.\n");
          login ())
       else (
         ANSITerminal.(print_string [cyan]
                         "Please enter a password: ");
         let created_password = String.trim(read_line ()) in
         let init_ctx = init () in 
         update_string init_ctx created_password; 
         let hashed_password = to_hex (finalize init_ctx) in
         let _ = Server.create_user created_username hashed_password in 
         ANSITerminal.(erase Screen);
         (ANSITerminal.(print_string [green] 
                          ("\n Hi "^created_username^", welcome to Essenger."))
         );
         main created_username ())
      )
    else(
      ANSITerminal.(print_string [red] ("\nUnrecognized. Please try again."));
      login ()
    )
  )

let () = login ()