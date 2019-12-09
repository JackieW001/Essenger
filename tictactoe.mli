(** This is a messenger-based version of the game Tic Tac Toe. A user can
    play by entering the command tictactoe, then 2 more fields:
    [user] should be the name of an existing user. 
    [new?] is optional; if "new" is included then a new game will be started 
    between the two players, otherwise the last game between the players
    will be continued (if it exists). If the "new" keyword is left out, but 
    there is no previous game history between the two players, a new game will
    be started.*)

(** The type for a Tic Tac Toe game.*)
type game = {
  u0: string;
  u1: string;
  state: int ref;
  board: string array ref;
  u0_moves: int list ref;
  u1_moves: int list ref;
  win: bool ref
}

(** [win winner game] prints the winner message if there is one and returns the
    game regardless of whether someone has won. *)
val win: string -> game -> game 

(** [move game] reads user input for which game square they want to take. *)
val move: game -> game

(** [valid_move square st game] adds the current user's move to their move list 
    and checks if the current user has won the game. *)
val valid_move: int -> int -> game -> game

(** [update_board board game] updates the overall [board] with each user's 
    moves. *)
val update_board: string array -> game -> unit

(** [print_board board] prints the game board. *)
val print_board: string array -> unit

(** [check_winner user moves game] checks whether the user's move list contains
    a win combination. For example, if [u0] 's move list contains [1;2;3], then
    they would win.  *)
val check_winner: string -> int list -> game -> game

(** [intro user0 user1] prints the game instructions and starts a new [game].*)
val intro: string -> string -> game

(** [string_of_game game] returns a string representation of the [game]. *)
val string_of_game: game -> string

