type game = {
  u0: string;
  u1: string;
  state: int ref;
  board: string array ref;
  u0_moves: int list ref;
  u1_moves: int list ref;
  win: bool ref
}

(** [string_of_list str] returns a string representation of a list. *)
let rec string_of_list str = function
  | [] -> str
  | h::t -> string_of_list (str ^ ";" ^ (string_of_int h)) t

(** [string_of_array board] returns an array representation of a game board. *)
let string_of_array board = 
  let str = ref "" in
  for i=0 to (board |> Array.length) - 1 do
    str := !str ^ ";" ^ board.(i)
  done;
  !str

let string_of_game game = 
  "u0:" ^ game.u0 ^ "\n" ^
  "u1:" ^ game.u1 ^ "\n" ^
  "state:" ^ string_of_int !(game.state) ^ "\n" ^
  "board:" ^ string_of_array !(game.board) ^ "\n" ^
  "u0_moves:" ^ string_of_list "" !(game.u0_moves) ^ "\n" ^
  "u1_moves:" ^ string_of_list "" !(game.u1_moves) ^ "\n" ^
  "win:" ^ string_of_bool !(game.win) ^ "\n"

let win winner game = 
  if winner <> "" then (
    if winner="tie" then (
      ANSITerminal.(print_string [green] ("\n You tied :( ) \n"));
      ANSITerminal.(print_string [white] "\n ----- GAME OVER ----- \n");
      ANSITerminal.(print_string [white] ("To start a new game with this user, 
      type @tictactoe " ^ (if (List.length !(game.u0_moves) = 5) 
                           then game.u1 else game.u0) ^ " new \n" ));
      game.win:=true; game)
    else (
      ANSITerminal.(print_string [green] ("\n" ^ winner ^ " wins!!!\n"));
      ANSITerminal.(print_string [white] "\n ----- GAME OVER ----- \n");
      ANSITerminal.(print_string [white] ("To start a new game with this user, 
      type @tictactoe " ^ (if winner = game.u0 then game.u1 else game.u0) 
                                          ^ " new \n" ));
      game.win := true; game))
  else game

let rec move game = 
  ANSITerminal.(print_string [cyan] "What square would you like to take? Enter a
  digit between 1 and 9.");
  let input = read_line () in
  match int_of_string_opt input with
  | None -> (
      ANSITerminal.(print_string [cyan] "Oops! You entered something other than 
      a digit between 1 and 9. Please try again.");
      move game
    )
  | Some i -> if (i >= 1 && i <= 9) then (
      if not (List.mem i !(game.u0_moves) && List.mem i !(game.u1_moves)) then
        (valid_move i !(game.state) game)
      else (ANSITerminal.(print_string [cyan] "That square has already been
      taken. Please try again. \n");
            move game))
    else (ANSITerminal.(print_string [cyan] "Oops! You entered an invalid 
    number. Please try again.");
          move game)

and valid_move square st game =
  match st with
  | 0 -> (
      game.u0_moves := square::!(game.u0_moves);
      game.state := 1;
      update_board !(game.board) game;
      check_winner game.u0 !(game.u0_moves) game
    )
  | 1 -> (
      game.u1_moves := square::!(game.u1_moves);
      game.state := 0;
      update_board !(game.board) game;
      check_winner game.u1 !(game.u1_moves) game
    )
  | _ -> failwith "INVALID STATE"
and update_board board game = 
  for i=0 to 8 do
    if List.mem (i+1) !(game.u0_moves) then board.(i) <- "O"
    else if List.mem (i+1) !(game.u1_moves) then board.(i) <- "X"
    else board.(i) <- string_of_int (i+1)
  done;
  game.board := board;
  print_board board
and print_board board = 
  ANSITerminal.(print_string [cyan] "\n Here is the current game board: ");
  ANSITerminal.(print_string [white] "\n -------------\n");
  for i=1 to 3 do
    ANSITerminal.(print_string [white] (" | " ^ board.(i-1) ))
  done;
  ANSITerminal.(print_string [white] " | \n");
  for i=4 to 6 do
    ANSITerminal.(print_string [white] (" | " ^ board.(i-1) ))
  done;
  ANSITerminal.(print_string [white] " | \n");
  for i=7 to 9 do
    ANSITerminal.(print_string [white] (" | " ^ board.(i-1)))
  done;
  ANSITerminal.(print_string [white] " | \n -------------\n ")
and check_winner user moves game = 
  if (List.mem 1 moves) && (List.mem 2 moves) && (List.mem 3 moves)
  then win user game
  else
  if (List.mem 4 moves) && (List.mem 5 moves) && (List.mem 6 moves)
  then win user game
  else
  if (List.mem 7 moves) && (List.mem 8 moves) && (List.mem 9 moves)
  then win user game
  else 
  if (List.mem 1 moves) && (List.mem 4 moves) && (List.mem 7 moves)
  then win user game
  else
  if (List.mem 2 moves) && (List.mem 5 moves) && (List.mem 8 moves)
  then win user game
  else
  if (List.mem 3 moves) && (List.mem 6 moves) && (List.mem 9 moves)
  then win user game
  else
  if (List.mem 1 moves) && (List.mem 5 moves) && (List.mem 9 moves)
  then win user game
  else
  if (List.mem 3 moves) && (List.mem 5 moves) && (List.mem 7 moves)
  then win user game
  else if (List.length moves = 5) then win "tie" game 
  else win "" game

let intro u0 u1 =
  ANSITerminal.(print_string [cyan] "\n Welcome to Tic Tac Toe! \n
  ------------------ \n");
  ANSITerminal.(print_string [cyan] "\n How to Play: \n");
  ANSITerminal.(print_string [cyan] "The game board is displayed below. The
  squares are numbered 1-9. When prompted, enter the number of the square 
  in which you would like to place your marker.");
  let game = {
    u0 = u0;
    u1 = u1;
    state = ref 0;
    board = ref [|"-1";"-1";"-1";"-1";"-1";"-1";"-1";"-1";"-1";|];
    u0_moves = ref [];
    u1_moves = ref [];
    win = ref false;
  } in
  update_board !(game.board) game;
  move game

