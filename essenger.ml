open Command
open Server 
open Sha256

(* Helper Functions *)

(** [substring contains s1 s2] returns true if s2 is a substring in s1. *)
let substring_contains s1 s2 = 
  let regexp = Str.regexp_string s2 in
  try ignore (Str.search_forward regexp s1 0); true
  with Not_found -> false


(** [main] is the main interface for Essenger. It takes parsed commands from 
    the command module and processes them to perform the proper function as 
    specified by the command. *)
let rec main current_user () = 
  print_newline();
  ANSITerminal.(print_string [cyan] "\nEssenger\n");
  ANSITerminal.(print_string [white] 
                  "Type @Help or @help to view supported commands.\n");
  print_string "> ";
  try 
    let command = read_line () |> Command.parse in
    match command with
    | Send (r,m) -> (* Send message to server *) 
      ANSITerminal.(print_string [cyan] 
                      ("Recipient: " ^ r ^ "\nMessage: " ^ m));
      if Server.conversation_exists current_user r then
        Server.add_msg current_user r m
      else 
        Server.create_conversation current_user r m;
      main current_user ()
    | Get r -> (* Get message history *) 
      ANSITerminal.(print_string [cyan] 
                      ("Getting message history with: " ^ r));
      ANSITerminal.(print_string [red] 
                      ("\nUnimplemented."));
      main current_user ()
    | Friends -> (* Get List of friends *) 
      ANSITerminal.(print_string [cyan] 
                      ("Getting friends list."));
      ANSITerminal.(print_string [red] 
                      ("\nUnimplemented."));
      main current_user ()
    | Help -> (* Access Help Options *) 
      ANSITerminal.(print_string [cyan]
                      "\nSupported Commands: \n
              @<username> : Gets your message history with <username>\n
              @<username> <message> : Sends <message> to user <username>\n
              @Friends or @friends : View who you've had conversations with.\n
              @Logout or @logout : Log out of Essenger.\n");
      main current_user ()
    | Logout -> (* Logout From Account *) 
      ANSITerminal.(print_string [cyan] 
                      "\nLogging Out.\n");
      exit 0
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
  print_newline ();
  ANSITerminal.(print_string [cyan] 
                  "\nWelcome to Essenger, the Better Messenger.\n");
  ANSITerminal.(print_string [cyan] 
                  "\nAre you a returning user? [y/n]");
  let response = read_line () in
  if response = "y" then (
    print_string "\nEnter your username: ";
    let username_input = String.trim(read_line ()) in
    print_string "Enter your password: ";
    let password_input = String.trim(read_line ()) in
    (* 
    if authenticated, then main, else try again
  *)
    if Server.auth username_input password_input then 
      (ANSITerminal.(print_string [green] 
                       ("\n"^username_input^", welcome to Essenger."));
       main username_input ())
    else
      ANSITerminal.(print_string [red] "\nIncorrect login, try again.");
    login ())
  else(
    if response = "n" then(
      ANSITerminal.(print_string [white] 
                      "\nPlease enter a username: ");
      let created_username = String.trim (read_line ()) in
      if Server.user_exists created_username then(
        ANSITerminal.(print_string [red]
                        "\nUsername already exists. Please try again.\n");
        login ()
      )
      else (
        ANSITerminal.(print_string [white]
                        "Please enter a password: ");
        let created_password = String.trim(read_line ()) in
        let init_ctx = init () in 
        update_string init_ctx created_password; 
        let hashed_password = to_hex (finalize init_ctx) in
        Server.create_user created_username hashed_password |> Lwt_main.run;
        (ANSITerminal.(print_string [green] 
                         ("\n"^created_username^", welcome to Essenger.")));
        main created_username ())
    )
    else(
      ANSITerminal.(print_string [red] ("\nUnrecognized. Please try again."));
      login ()
    )
  )

let () = login ()