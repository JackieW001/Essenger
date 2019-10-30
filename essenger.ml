open Command

let rec main () = 
  print_newline();
  print_string "> ";
  try 
    let command = read_line () |> Command.parse in
    match command with
    | Send (r,m) -> (* Send message to server *) main ()
    | Get r -> (* Get message history *) main ()
    | Help -> (* Access Help Options *) main ()
    | Logout -> (* Logout From Account *) 
      ANSITerminal.(print_string [blue] 
                      "\nLogging Out.\n");
      exit 0
  with
  | Malformed -> print_string "Please Try Again.";
    main()
  | Empty -> print_string "Please Try Again.";
    main()

let login () = 
  print_newline();
  ANSITerminal.(print_string [blue] 
                  "\nWelcome to Essenger, the Better Messenger.\n");
  print_string "Enter your username: ";
  let username_input = read_line () in
  print_string "Enter your password: ";
  let password_input = read_line () in
  (* 
    if authenticated, then main, else try again
  *)
  print_string ("Testing: " ^ username_input ^ " "^ password_input);
  ANSITerminal.(print_string [blue] 
                  "\nWelcome to Essenger.\n");
  main()

let () = login ()