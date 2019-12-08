type game = {
  u0: string;
  u1: string;
  state: int ref;
  board: string array ref;
  u0_moves: int list ref;
  u1_moves: int list ref;
  win: bool ref
}

val win: string -> game -> game 
val move: game -> game
val valid_move: int -> int -> game -> game
val update_board: string array -> game -> unit
val print_board: string array -> unit
val check_winner: string -> int list -> game -> game
val intro: string -> string -> game
val string_of_game: game -> string

