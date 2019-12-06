(** [state] tracks which player's turn it is. [state 0] means it is O's turn.
    [state 1] means it is X's turn. =  *)
let state = ref 0
let current_board = ref [|"-1";"-1";"-1";"-1";"-1";"-1";"-1";"-1";"-1";|]

let win = ref false

let u0_moves = ref []
let u1_moves = ref []

let win winner = 
  if winner <> "" then (
    ANSITerminal.(print_string [green] ("\n" ^ winner ^ " wins!!!\n"));
    ANSITerminal.(print_string [white] "\n ----- GAME OVER ----- \n");
    exit 0)
  else print_newline ()

let rec move st = 
  ANSITerminal.(print_string [cyan] "What square would you like to take? Enter a
  digit between 1 and 9.");
  let input = read_line () in
  match int_of_string_opt input with
  | None -> (
      ANSITerminal.(print_string [cyan] "Oops! You entered something other than a
    digit between 1 and 9. Please try again.");
      move st
    )
  | Some i -> if (i >= 1 && i <= 9) then (
      if not (List.mem i !u0_moves && List.mem i !u1_moves) then
        (valid_move i st; move !state)
      else (ANSITerminal.(print_string [cyan] "That square has already been
      taken. Please try again. \n");
            move st))
    else (ANSITerminal.(print_string [cyan] "Oops! You entered an invalid number.
  Please try again.");
          move st)
and valid_move square st =
  match st with
  | 0 -> (
      u0_moves := square::!u0_moves;
      state := 1;
      update_board !current_board;
      check_winner "u0" !u0_moves
    )
  | 1 -> (
      u1_moves := square::!u1_moves;
      state := 0;
      update_board !current_board;
      check_winner "u1" !u1_moves
    )
  | _ -> failwith "INVALID STATE"
(* (square, st) *)
and update_board board = 
  for i=0 to 8 do
    if List.mem (i+1) !u0_moves then board.(i) <- "O"
    else if List.mem (i+1) !u1_moves then board.(i) <- "X"
    else board.(i) <- string_of_int (i+1)
  done;
  current_board := board;
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
and check_winner user moves = 
  if (List.mem 1 moves) && (List.mem 2 moves) && (List.mem 3 moves)
  then win user
  else
  if (List.mem 4 moves) && (List.mem 5 moves) && (List.mem 6 moves)
  then win user
  else
  if (List.mem 7 moves) && (List.mem 8 moves) && (List.mem 9 moves)
  then win user
  else
  if (List.mem 1 moves) && (List.mem 4 moves) && (List.mem 7 moves)
  then win user
  else
  if (List.mem 2 moves) && (List.mem 5 moves) && (List.mem 8 moves)
  then win user
  else
  if (List.mem 3 moves) && (List.mem 6 moves) && (List.mem 9 moves)
  then win user
  else
  if (List.mem 1 moves) && (List.mem 5 moves) && (List.mem 9 moves)
  then win user
  else
  if (List.mem 3 moves) && (List.mem 5 moves) && (List.mem 7 moves)
  then win user
  else win ""

let intro =
  ANSITerminal.(print_string [cyan] "\n Welcome to Tic Tac Toe! \n
  ------------------ \n");
  ANSITerminal.(print_string [cyan] "\n How to Play: \n");
  ANSITerminal.(print_string [cyan] "The game board is displayed below. The
  squares are numbered 1-9. When prompted, enter the number of the square in which
  you would like to place your marker.");
  update_board !current_board;
  move !state

