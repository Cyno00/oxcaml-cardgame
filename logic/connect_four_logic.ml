open! Core

(** Connect Four game logic.

    Connect Four is played on a 6x7 grid where players alternate dropping colored
    discs into columns. Pieces fall to the lowest available position in the column.
    The first player to get 4 pieces in a row (horizontally, vertically, or diagonally)
    wins the game. *)

module Player_kind = struct
  type t =
    | Red
    | Yellow
  [@@deriving sexp, compare, equal]

  let opposite (t : t) : t =
    match t with
    | Red -> Yellow
    | Yellow -> Red
  ;;
end

module Cell_position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

(** A move in Connect Four is just selecting a column.
    The piece will fall to the lowest available row in that column. *)
module Move = struct
  type t = { column : int } [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Stalemate | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; decision : Decision.t
    ; last_move : Cell_position.t option (* The actual cell where the last piece landed *)
    }
  [@@deriving sexp, compare, equal]

  (** Standard Connect Four dimensions *)
  let default_rows = 6
  let default_columns = 7
  let winning_length = 4

  module Create_error = struct
    type t =
      | Board_too_big_or_small
      | Invalid_dimensions
    [@@deriving sexp, compare]
  end

  let create ~rows ~columns : (t, Create_error.t list) Result.t =
    let size_ok = rows > 0 && columns > 0 && rows < 20 && columns < 20 in
    let dimensions_ok = rows >= winning_length || columns >= winning_length in
    match size_ok, dimensions_ok with
    | true, true ->
      Ok
        { board = Cell_position.Map.empty
        ; rows
        ; columns
        ; decision = In_progress { whose_turn = Red }
        ; last_move = None
        }
    | _ ->
      Error
        ((if size_ok then [] else [ Create_error.Board_too_big_or_small ])
         @ if dimensions_ok then [] else [ Create_error.Invalid_dimensions ])
  ;;

  (** Creates a standard 6x7 Connect Four game *)
  let create_standard () : t =
    match create ~rows:default_rows ~columns:default_columns with
    | Ok game -> game
    | Error _ -> failwith "Failed to create standard Connect Four game"
  ;;

  (** Find the lowest empty row in a column (where the piece would fall to).
      Returns None if the column is full. *)
  let find_lowest_empty_row t column : int option =
    List.range 0 t.rows
    |> List.rev (* Start from bottom row *)
    |> List.find ~f:(fun row ->
      let position = { Cell_position.row; column } in
      not (Map.mem t.board position))
  ;;

  (** Check if a specific sequence of cells all contain the same player's piece *)
  let value_if_all_the_same list =
    match list with
    | hd :: tl -> if List.for_all tl ~f:(Player_kind.equal hd) then Some hd else None
    | [] -> None
  ;;

  (** Check for a winning sequence starting from a position in a specific direction *)
  let check_direction_starting_from
        ~vertical_delta
        ~horizontal_delta
        { board; rows; columns; _ }
        ({ row; column } : Cell_position.t)
    =
    let cells =
      List.range 0 winning_length
      |> List.filter_map ~f:(fun i ->
        let new_row = row + (i * vertical_delta) in
        let new_column = column + (i * horizontal_delta) in
        (* Ensure we stay within bounds *)
        if new_row >= 0 && new_row < rows && new_column >= 0 && new_column < columns
        then Map.find board { Cell_position.row = new_row; column = new_column }
        else None)
    in
    if List.length cells >= winning_length
    then value_if_all_the_same cells
    else None
  ;;

  (** All 8 possible directions to check for winning sequences *)
  let deltas = List.init 3 ~f:(fun i -> i - 1)

  let all_directions =
    List.cartesian_product deltas deltas
    |> List.filter ~f:(fun (vertical_delta, horizontal_delta) ->
      vertical_delta <> 0 || horizontal_delta <> 0)
  ;;

  (** Check all directions from a position for a winning sequence *)
  let check_all_directions t cell_position =
    all_directions
    |> List.filter_map ~f:(fun (vertical_delta, horizontal_delta) ->
      check_direction_starting_from ~vertical_delta ~horizontal_delta t cell_position)
    |> value_if_all_the_same
  ;;

  (** Check if there's a winner by examining all positions on the board *)
  let check_winner t =
    Map.filter_keys t.board ~f:(fun cell_position ->
      check_all_directions t cell_position |> Option.is_some)
    |> Map.min_elt
    |> Option.map ~f:snd
  ;;

  (** Check if a column index is valid *)
  let is_legal_column { columns; _ } column = 0 <= column && column < columns

  module Move_error = struct
    type t =
      | Game_is_over
      | Column_is_full
      | Illegal_column
    [@@deriving sexp, compare]
  end

  (** Get all legal moves (all non-full columns) *)
  let get_all_moves t : Move.t list =
    List.range 0 t.columns
    |> List.filter_map ~f:(fun column ->
      match find_lowest_empty_row t column with
      | Some _ -> Some { Move.column }
      | None -> None)
  ;;

  (** Make a move by dropping a piece in the specified column *)
  let make_move t ({ column } : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | _ when not (is_legal_column t column) -> Error Illegal_column
    | Winner _ | Stalemate -> Error Game_is_over
    | In_progress { whose_turn } ->
      (match find_lowest_empty_row t column with
       | None -> Error Column_is_full
       | Some row ->
         let cell_position = { Cell_position.row; column } in
         let board = Map.set t.board ~key:cell_position ~data:whose_turn in
         let decision : Decision.t =
           match check_winner { t with board } with
           | Some player_kind -> Winner player_kind
           | None ->
             (* Stalemate if board is full *)
             if Map.length board >= t.columns * t.rows
             then Stalemate
             else In_progress { whose_turn = Player_kind.opposite whose_turn }
         in
         Ok { t with board; decision; last_move = Some cell_position })
  ;;

  (** Get the player who occupies a specific cell, if any *)
  let get_cell t ({ row; column } : Cell_position.t) : Player_kind.t option =
    Map.find t.board { row; column }
  ;;

  (** Check if the board is full *)
  let is_board_full t = Map.length t.board >= t.rows * t.columns

  module For_testing = struct
    let all_directions = all_directions
    let find_lowest_empty_row = find_lowest_empty_row
  end
end

(** AI module for Connect Four using minimax with alpha-beta pruning *)
module AI = struct
  (** Count sequences of a specific length for a player in a direction *)
  let count_sequence_in_direction
        ~length
        ~vertical_delta
        ~horizontal_delta
        { Game_state.board; rows; columns; _ }
        ({ Cell_position.row; column } : Cell_position.t)
        player
    =
    let cells =
      List.range 0 length
      |> List.filter_map ~f:(fun i ->
        let new_row = row + (i * vertical_delta) in
        let new_column = column + (i * horizontal_delta) in
        if new_row >= 0 && new_row < rows && new_column >= 0 && new_column < columns
        then Some (Map.find board { Cell_position.row = new_row; column = new_column })
        else None)
    in
    if List.length cells >= length
       && List.for_all cells ~f:(function
         | Some p when Player_kind.equal p player -> true
         | None -> true
         | _ -> false)
       && List.exists cells ~f:(function
         | Some p when Player_kind.equal p player -> true
         | _ -> false)
    then 1
    else 0
  ;;

  (** Evaluate position value for a specific player *)
  let evaluate_position_for_player game player =
    let score = ref 0 in
    (* Check all positions and directions for sequences *)
    for row = 0 to game.Game_state.rows - 1 do
      for col = 0 to game.Game_state.columns - 1 do
        let pos = { Cell_position.row; column = col } in
        (* Only evaluate from positions occupied by this player *)
        match Game_state.get_cell game pos with
        | Some p when Player_kind.equal p player ->
          (* Check all 8 directions *)
          List.iter Game_state.For_testing.all_directions ~f:(fun (vd, hd) ->
            (* Sequences of 3 are very valuable (can become 4) *)
            score := !score + (count_sequence_in_direction ~length:3 ~vertical_delta:vd ~horizontal_delta:hd game pos player * 100);
            (* Sequences of 2 are somewhat valuable *)
            score := !score + (count_sequence_in_direction ~length:2 ~vertical_delta:vd ~horizontal_delta:hd game pos player * 10))
        | _ -> ()
      done
    done;
    (* Prefer center columns *)
    let center_col = game.Game_state.columns / 2 in
    for row = 0 to game.Game_state.rows - 1 do
      match Game_state.get_cell game { Cell_position.row; column = center_col } with
      | Some p when Player_kind.equal p player -> score := !score + 3
      | _ -> ()
    done;
    !score
  ;;

  (** Heuristic evaluation from Red's perspective (Red tries to maximize) *)
  let heuristic_value (game : Game_state.t) =
    match game.decision with
    | Decision.Stalemate -> 0
    | Decision.In_progress _ ->
      let red_score = evaluate_position_for_player game Red in
      let yellow_score = evaluate_position_for_player game Yellow in
      red_score - yellow_score
    | Decision.Winner player_kind ->
      (match player_kind with
       | Red -> 100000
       | Yellow -> -100000)
  ;;

  (** Generate children states, sorted by heuristic value for better pruning *)
  let children game ~(maximizing_player : bool) =
    let compare = if maximizing_player then Int.descending else Int.ascending in
    let moves = Game_state.get_all_moves game in
    List.filter_map moves ~f:(fun move -> Game_state.make_move game move |> Result.ok)
    |> List.sort ~compare:(Comparable.lift ~f:heuristic_value compare)
  ;;

  (** Minimax with alpha-beta pruning *)
  let rec alpha_beta_search (game : Game_state.t) depth alpha beta maximizing_player =
    match game.decision with
    | Decision.In_progress _ when depth > 0 ->
      if maximizing_player
      then (
        (* Maximizing player (Red) *)
        let value = ref Int.min_value in
        let alpha = ref alpha in
        let children = children game ~maximizing_player:true in
        let continue = ref true in
        List.iter children ~f:(fun child ->
          if !continue
          then (
            let child_value =
              alpha_beta_search child (depth - 1) !alpha beta false
            in
            value := Int.max !value child_value;
            alpha := Int.max !alpha !value;
            if !value >= beta then continue := false));
        !value)
      else (
        (* Minimizing player (Yellow) *)
        let value = ref Int.max_value in
        let beta = ref beta in
        let children = children game ~maximizing_player:false in
        let continue = ref true in
        List.iter children ~f:(fun child ->
          if !continue
          then (
            let child_value = alpha_beta_search child (depth - 1) alpha !beta true in
            value := Int.min !value child_value;
            beta := Int.min !beta !value;
            if !value <= alpha then continue := false));
        !value)
    | _ -> heuristic_value game
  ;;

  (** Get the best move for the current player using alpha-beta search *)
  let get_best_move (game : Game_state.t) ~(depth : int) : Move.t option =
    match game.decision with
    | Decision.Winner _ | Decision.Stalemate -> None
    | Decision.In_progress { whose_turn } ->
      let moves = Game_state.get_all_moves game in
      let maximizing_player =
        match whose_turn with
        | Red -> true
        | Yellow -> false
      in
      let moves_and_values =
        List.filter_map moves ~f:(fun move ->
          match Game_state.make_move game move with
          | Ok child ->
            let value =
              alpha_beta_search child (depth - 1) Int.min_value Int.max_value (not maximizing_player)
            in
            Some (move, value)
          | Error _ -> None)
      in
      let best_move =
        (if maximizing_player then List.max_elt else List.min_elt)
          moves_and_values
          ~compare:(fun (_move, v1) (_move, v2) -> Int.compare v1 v2)
        |> Option.map ~f:(fun (move, _value) -> move)
      in
      best_move
  ;;
end
