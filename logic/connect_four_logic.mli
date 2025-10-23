open! Core

(** Connect Four game logic.

    Connect Four is played on a 6x7 grid where players alternate dropping colored
    discs into columns. Pieces fall to the lowest available position in the column.
    The first player to get 4 pieces in a row (horizontally, vertically, or diagonally)
    wins the game. *)

module Player_kind : sig
  type t =
    | Red
    | Yellow
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Cell_position : sig
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, compare]

  (* Defines a [Cell_position.Map.t]. *)
  include Comparable.S with type t := t
end

(** A move in Connect Four is selecting a column to drop a piece into. *)
module Move : sig
  type t = { column : int } [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; decision : Decision.t
    ; last_move : Cell_position.t option (* The actual cell where the last piece landed *)
    }
  [@@deriving sexp, compare, equal]

  (** Standard Connect Four dimensions *)
  val default_rows : int

  val default_columns : int
  val winning_length : int

  module Create_error : sig
    type t =
      | Board_too_big_or_small
      | Invalid_dimensions
    [@@deriving sexp, compare]
  end

  (** Create a Connect Four game with custom dimensions *)
  val create : rows:int -> columns:int -> (t, Create_error.t list) Result.t

  (** Creates a standard 6x7 Connect Four game *)
  val create_standard : unit -> t

  module Move_error : sig
    type t =
      | Game_is_over
      | Column_is_full
      | Illegal_column
    [@@deriving sexp, compare]
  end

  (** Get all legal moves (columns that are not full) *)
  val get_all_moves : t -> Move.t list

  (** Make a move by dropping a piece in the specified column *)
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  (** Get the player who occupies a specific cell, if any *)
  val get_cell : t -> Cell_position.t -> Player_kind.t option

  (** Check if the board is completely full *)
  val is_board_full : t -> bool

  module For_testing : sig
    val all_directions : (int * int) list
    val find_lowest_empty_row : t -> int -> int option
  end
end

(** AI module for Connect Four using minimax with alpha-beta pruning *)
module AI : sig
  (** Get the best move for the current player.

      Uses minimax algorithm with alpha-beta pruning to search the game tree.
      The [depth] parameter controls how many moves ahead to look.

      Recommended depths:
      - depth 1-3: Fast, easy difficulty
      - depth 4-6: Medium difficulty (good balance)
      - depth 7+: Hard difficulty (may be slow)

      Returns [None] if the game is already over. *)
  val get_best_move : Game_state.t -> depth:int -> Move.t option
end
