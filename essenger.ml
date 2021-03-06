open Command
open Server 
open Sha256

(* Helper Functions *)

(** The list of available stickers. *)
let stickers = [
  (1, {|(O-O)|}); 
  (2, {|(\^o^/)|});
  (3, {|( .o.)|});
  (4, {|( .-. )|});
  (5, {|( -.- )|})
]

(** The list of available emojis. *)
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

(** [print_stickers] prints the list of available stickers and each associated 
    number. *)
let rec print_stickers = function
  | [] -> ANSITerminal.(print_string [cyan] "")
  | (id, s)::t -> ANSITerminal.(print_string [white] s); 
    ANSITerminal.(print_string [cyan] (String.concat (string_of_int id) [" "; "\n"]));
    print_stickers t

(** [print_emojis] prints the list of available emojis and each associated name.*)
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


(** [arr_of_board arr i] returns an array representation of a the board string. *)
let rec arr_of_board arr i = function
  | [] -> arr
  | h::t -> arr.(i) <- h; arr_of_board arr (i+1) t

(** [list_of_moves lst] returns a list representation of the user moves string. *)
let rec list_of_moves lst = function
  | [] -> lst
  | h::t -> list_of_moves ((int_of_string h)::lst) t

(** [parse_game_string arr i] returns an array representation of the game. *)
let rec parse_game_string arr i = function
  | [] -> arr
  | h::t -> (
      let index = Str.search_forward (Str.regexp {|\\((:\w*\n))\\|}) h 0 in 
      let matched = String.sub h (index+1) (String.length h - (index+1)) in 
      let value = Str.matched_string matched in
      arr.(i) <- value;
      print_string value;
      parse_game_string arr (i+1) t
    )

(** [game_of_string s] returns a game of type [Tictactoe.game].*)
let game_of_string s = 
  print_string "reached game of string";
  let split_game = Str.split (Str.regexp "\\[ \n]\\") s in
  let game_array = parse_game_string [|""; ""; ""; "";""; ""; ""|] 0 split_game in
  let game : Tictactoe.game = {
    u0 = game_array.(0);
    u1 = game_array.(1);
    state = ref (int_of_string game_array.(2));
    board = ref (Str.split (Str.regexp "\\[;]\\") game_array.(3) 
                 |> arr_of_board [|""; ""; ""; "";""; ""; ""; ""; ""|] 0);
    u0_moves = ref ((Str.split (Str.regexp "\\[;]\\") game_array.(4))
                    |> list_of_moves []);
    u1_moves = ref ((Str.split (Str.regexp "\\[;]\\") game_array.(5))
                    |> list_of_moves []);
    win = ref (bool_of_string game_array.(6))
  } in
  game


(** [valid_gc_member gc n] returns true if [gc] exists and [n] is a member of 
    the [gc], false otherwise. *)
let valid_gc_member gc n =
  ((gc_exists gc) && (List.mem n (get_gc_users gc)))

(* Main Program *)

(** [main] is the main interface for Essenger. It takes parsed commands from 
    the command module and processes them to perform the proper function as 
    specified by the command. *)
let rec main current_user ()= 
  print_newline();
  ANSITerminal.(print_string [cyan] "\nEssenger\n");
  if (List.length (get_notifications current_user) != 0) then
    (ANSITerminal.(print_string [cyan] "\nYou have new messages from: \n");
     print_list (get_notifications current_user);)
  else print_string "";
  ANSITerminal.(print_string [white] 
                  "Type @Help or @help to view supported commands.\n");
  print_string "> ";
  try 
    let command = read_line () |> Command.parse in
    match command with
    | Send (r,m) -> (* Send message to server *) 
      if (Server.user_exists r) then (
        ANSITerminal.(print_string [cyan] 
                        ("Recipient: " ^ r ^ "\nMessage: " ^ m ^ "\n"));
        Server.add_msg current_user r m;
        main current_user ()
      )
      else (
        ANSITerminal.(print_string [red]
                        ("\nUser \""^r^"\" does not exist.\n"));
        main current_user ()
      )
    | Get r ->( (* Get message history *) 
        try
          if Server.user_exists r then 
            (ANSITerminal.(print_string [cyan] "\nMessages: \n");
             print_list (List.rev 
                           (Server.get_conversation_history current_user r)); 
             main current_user ())
          else 
            (ANSITerminal.(print_string [red]
                             ("\nUser " ^ r ^ " does not exist.\n"));
             main current_user ())
        with
        | Failure x -> 
          ANSITerminal.(print_string [red]
                          ("\nYou have no message history with " ^ r ^ "\n"));
          main current_user ())
    | Friends -> (* Get List of friends *) 
      print_endline "";
      Server.get_friends current_user |> print_list;
      print_endline "-------------------------------";
      (*ANSITerminal.(print_string [red] 
                      ("\nUnimplemented.")); *)
      main current_user ()
    | Help -> (* Access Help Options *) 
      ANSITerminal.(print_string [cyan]
                      "\nSupported Commands:

              ~ Direct Messaging Commands ~");
      ANSITerminal.(print_string [white] "
              @<username> : Gets your message history with <username>\n
              @<username> <message> : Sends <message> to user <username>\n
              @Friends or @friends : View who you've had conversations with.\n
              @Logout or @logout : Log out of Essenger.\n
              ");
      ANSITerminal.(print_string [cyan] "~ GroupChat Commands ~\n");
      ANSITerminal.(print_string [white] 
                      "              @gc <gc_name> <username list> : 
                                Create a GroupChat with users specified in 
                                <username list>. Every word separated by spaces 
                                after <gc_name> specifies a user to add to the 
                                GroupChat.\n
              @gcget <gc_name> : Get message history of GroupChat <gc_name>.\n
              @gcsend <gc_name> <message> : Sends <message> to GroupChat 
                  <gc_name> \n
              @gcadd <gc_name> <username list> : Adds users in <username list>
                  to <gc_name>. <gc_name> must exist and you must be a member
                  of <gc_name>. \n");

      ANSITerminal.(print_string [cyan] 
                      "                                                       
              ~ Emojis and Stickers ~");
      ANSITerminal.(print_string [white] 
                      "
              @Emojis or @emojis : View available emojis.\n
              @Stickers or @stickers : View available stickers.\n");
      ANSITerminal.(print_string [cyan] "\nEmojis and Stickers:\n");
      ANSITerminal.(print_string [white] "
              Send emojis and stickers in your messages by typing 
              #<emoji or sticker name> in your message as a word.\n
              Type @emojis or @stickers to view available stickers and emojis.
              \n");

      main current_user ()
    | Logout -> (* Logout From Account *) 
      ANSITerminal.(print_string [cyan] 
                      "\nLogging Out.\n");
      exit 0
    | Sticker -> 
      ANSITerminal.(print_string [cyan] "\nAvailable stickers:\n");
      print_stickers stickers;
      ANSITerminal.(print_string [cyan] ("\nTo send a sticker, enter: "^
                                         "\n@username #[sticker number].\n"));
      main current_user ()
    | Emojis ->
      ANSITerminal.(print_string [cyan] "\nAvailable emojis: \n");
      print_emojis emojis;
      ANSITerminal.(print_string 
                      [cyan] 
                      "\nTo use an emoji, enter: \n@username #[emoji name].\n");
      main current_user ()
    | GroupChat (n, ht) -> 
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
    | Tictactoe (user, newgame) -> (
        let game_prev : Tictactoe.game option = 
          Some {u0 = "u0";
                u1 = "u1";
                state = ref 0;
                board = ref [|"-1";"-1";"-1";"-1";"-1";"-1";"-1";"-1";"-1";|];
                u0_moves = ref [];
                u1_moves = ref [];
                win = ref false;} in
        match game_prev with
        | None -> (
            print_string "reached";
            ANSITerminal.(print_string [cyan] 
                            ("Starting Tic Tac Toe with " ^ user));
            let game = Tictactoe.intro current_user user in
            if (Server.user_exists user) then (
              ANSITerminal.(print_string [cyan] 
                              ("Recipient: " ^ user ^ "\nBoard: "));
              (!(game.board) |> Tictactoe.print_board));
            Server.add_msg current_user user (game |> Tictactoe.string_of_game);
            main current_user ()
          )
        | Some g -> (
            if (newgame = "new") || !(g.win)  then (
              ANSITerminal.(print_string [cyan] 
                              ("Starting Tic Tac Toe with " ^ user));
              let game = Tictactoe.intro current_user user in
              if (Server.user_exists user) then (
                ANSITerminal.(print_string [cyan] 
                                ("Recipient: " ^ user ^ "\nBoard: "));
                (!(game.board) |> Tictactoe.print_board));
              Server.add_msg current_user user 
                (game |> Tictactoe.string_of_game);
              main current_user ()
            ) else (
              ANSITerminal.(print_string [cyan] 
                              ("Continuing Tic Tac Toe with " ^ user));
              let game = Tictactoe.move g in
              if (Server.user_exists user) then (
                ANSITerminal.(print_string [cyan] 
                                ("Recipient: " ^ user ^ "\nBoard: "));
                (!(game.board) |> Tictactoe.print_board));
              Server.add_msg current_user user 
                (game |> Tictactoe.string_of_game);
              main current_user ())
          )
      )
    | GroupChatGet n -> 
      if valid_gc_member n current_user then
        ( 
          ANSITerminal.(print_string [cyan] "\nMembers: \n");
          print_list (get_gc_users n);
          ANSITerminal.(print_string [cyan] "Messages: \n");
          print_list (get_gc_history n |> List.rev);
          main current_user ())
      else 
      if (not (gc_exists n)) then
        (ANSITerminal.(print_string [red] 
                         ("\nThis GroupChat does not exist.\n"));
         main current_user ())
      else
        (ANSITerminal.(print_string [red] 
                         ("\nYou are not a member of this GroupChat.\n"));
         main current_user ())
    | GroupChatSend (n,m) -> 
      if valid_gc_member n current_user then
        (add_gc_msg n current_user m;
         ANSITerminal.(print_string [cyan] 
                         ("GroupChat: " ^ n ^ "\nMessage: " ^ m ^"\n"));
         main current_user ())
      else 
      if (not (gc_exists n)) then 
        (ANSITerminal.(print_string [red] 
                         ("\nThis GroupChat does not exist.\n"));
         main current_user ())
      else
        (ANSITerminal.(print_string [red] 
                         ("\nYou are not a member of this GroupChat.\n"));
         main current_user ()
        )
    | GroupChatAdd (n,ht) ->
      if valid_gc_member n current_user then
        (add_users_to_gc n ht;
         ANSITerminal.(print_string [green] 
                         ("Members Added: "));
         print_list ht;
         main current_user ())
      else
      if (not (gc_exists n)) then 
        (ANSITerminal.(print_string [red] 
                         ("\nThis GroupChat does not exist.\n"));
         main current_user ())
      else
        (ANSITerminal.(print_string [red] 
                         ("\nYou are not a member of this GroupChat.\n"));
         main current_user ()
        )
  with
  | Malformed -> print_string (
      "Invalid input. Please try again. " ^ 
      "\nType @help or @Help to see command structures. \n");
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
  print_endline ({|

 /$$$$$$$$  /$$$$$$   /$$$$$$  /$$$$$$$$ /$$   /$$  /$$$$$$  /$$$$$$$$ /$$$$$$$ 
| $$_____/ /$$__  $$ /$$__  $$| $$_____/| $$$ | $$ /$$__  $$| $$_____/| $$__  $$
| $$      | $$  \__/| $$  \__/| $$      | $$$$| $$| $$  \__/| $$      | $$  \ $$
| $$$$$   |  $$$$$$ |  $$$$$$ | $$$$$   | $$ $$ $$| $$ /$$$$| $$$$$   | $$$$$$$/
| $$__/    \____  $$ \____  $$| $$__/   | $$  $$$$| $$|_  $$| $$__/   | $$__  $$
| $$       /$$  \ $$ /$$  \ $$| $$      | $$\  $$$| $$  \ $$| $$      | $$  \ $$
| $$$$$$$$|  $$$$$$/|  $$$$$$/| $$$$$$$$| $$ \  $$|  $$$$$$/| $$$$$$$$| $$  | $$
|________/ \______/  \______/ |________/|__/  \__/ \______/ |________/|__/  |__/

                                                                        |} );
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