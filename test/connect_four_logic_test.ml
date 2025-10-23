open! Core
open Tictactoe_logic_library
open Connect_four_logic

let ok_exn result = Result.ok result |> Option.value_exn

(* Helper to create a standard game *)
let standard_game () = Game_state.create_standard ()

(* Helper to print game state *)
let print_game_state state =
  print_s [%sexp (state : Game_state.t)]
;;

(* Helper to make a move and print result *)
let make_move_and_print game_state column =
  let result = Game_state.make_move game_state { Move.column } in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

(* Helper to create and print a game *)
let create_and_print ~rows ~columns =
  let result = Game_state.create ~rows ~columns in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]
;;

(* Helper to make multiple moves *)
let make_moves game moves =
  List.fold moves ~init:game ~f:(fun game column ->
    Game_state.make_move game { Move.column } |> ok_exn)
;;

let%test "Standard game creation" =
  let state = Game_state.create_standard () in
  state.rows = 6 && state.columns = 7 && Map.is_empty state.board
;;

let%expect_test "Create standard 6x7 Connect Four game" =
  print_game_state (standard_game ());
  [%expect
    {|
    ((board ()) (rows 6) (columns 7) (decision (In_progress (whose_turn Red)))
     (last_move ()))
    |}]
;;

let%expect_test "Game_state.create with custom dimensions" =
  create_and_print ~rows:6 ~columns:7;
  [%expect
    {|
    (Ok
     ((board ()) (rows 6) (columns 7) (decision (In_progress (whose_turn Red)))
      (last_move ())))
    |}];
  create_and_print ~rows:10 ~columns:10;
  [%expect
    {|
    (Ok
     ((board ()) (rows 10) (columns 10) (decision (In_progress (whose_turn Red)))
      (last_move ())))
    |}]
;;

let%expect_test "Game_state.create fails on invalid sizes" =
  create_and_print ~rows:0 ~columns:7;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~rows:6 ~columns:0;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~rows:25 ~columns:7;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~rows:2 ~columns:2;
  [%expect {| (Error (Invalid_dimensions)) |}]
;;

let%expect_test "First move drops to bottom row" =
  let game = standard_game () in
  make_move_and_print game 3;
  [%expect
    {|
    (Ok
     ((board ((((row 5) (column 3)) Red))) (rows 6) (columns 7)
      (decision (In_progress (whose_turn Yellow)))
      (last_move (((row 5) (column 3))))))
    |}]
;;

let%expect_test "Multiple moves in same column stack up" =
  let game = standard_game () in
  let game = Game_state.make_move game { column = 0 } |> ok_exn in
  let game = Game_state.make_move game { column = 0 } |> ok_exn in
  make_move_and_print game 0;
  [%expect
    {|
    (Ok
     ((board
       ((((row 3) (column 0)) Red) (((row 4) (column 0)) Yellow)
        (((row 5) (column 0)) Red)))
      (rows 6) (columns 7) (decision (In_progress (whose_turn Yellow)))
      (last_move (((row 3) (column 0))))))
    |}]
;;

let%expect_test "Cannot play in full column" =
  let game = standard_game () in
  (* Fill column 0 with 6 pieces *)
  let game = make_moves game [ 0; 0; 0; 0; 0; 0 ] in
  (* Try to add a 7th piece *)
  make_move_and_print game 0;
  [%expect {| (Error Column_is_full) |}]
;;

let%expect_test "Cannot play in invalid column" =
  let game = standard_game () in
  make_move_and_print game 7;
  [%expect {| (Error Illegal_column) |}];
  make_move_and_print game (-1);
  [%expect {| (Error Illegal_column) |}]
;;

let%expect_test "Horizontal win detection" =
  let game = standard_game () in
  (* Red plays columns 0, 1, 2
     Yellow plays columns 4, 5, 6
     Red plays column 3 to win horizontally *)
  let game = make_moves game [ 0; 4; 1; 5; 2; 6 ] in
  make_move_and_print game 3;
  [%expect
    {|
    (Ok
     ((board
       ((((row 5) (column 0)) Red) (((row 5) (column 1)) Red)
        (((row 5) (column 2)) Red) (((row 5) (column 3)) Red)
        (((row 5) (column 4)) Yellow) (((row 5) (column 5)) Yellow)
        (((row 5) (column 6)) Yellow)))
      (rows 6) (columns 7) (decision (Winner Red))
      (last_move (((row 5) (column 3))))))
    |}]
;;

let%expect_test "Vertical win detection" =
  let game = standard_game () in
  (* Red plays column 3 four times (with Yellow playing column 4 in between) *)
  let game = make_moves game [ 3; 4; 3; 4; 3; 4 ] in
  make_move_and_print game 3;
  [%expect
    {|
    (Ok
     ((board
       ((((row 2) (column 3)) Red) (((row 3) (column 3)) Red)
        (((row 3) (column 4)) Yellow) (((row 4) (column 3)) Red)
        (((row 4) (column 4)) Yellow) (((row 5) (column 3)) Red)
        (((row 5) (column 4)) Yellow)))
      (rows 6) (columns 7) (decision (Winner Red))
      (last_move (((row 2) (column 3))))))
    |}]
;;


let%expect_test "Cannot play after game is won" =
  let game = standard_game () in
  (* Create a horizontal win for Red *)
  let game = make_moves game [ 0; 4; 1; 5; 2; 6; 3 ] in
  (* Game should be won by Red, try to make another move *)
  make_move_and_print game 4;
  [%expect {| (Error Game_is_over) |}]
;;

let%expect_test "Get all legal moves from empty board" =
  let game = standard_game () in
  let moves = Game_state.get_all_moves game in
  print_s [%sexp (moves : Move.t list)];
  [%expect
    {|
    (((column 0)) ((column 1)) ((column 2)) ((column 3)) ((column 4))
     ((column 5)) ((column 6)))
    |}]
;;

let%expect_test "Get all legal moves excludes full columns" =
  let game = standard_game () in
  (* Fill column 0 *)
  let game = make_moves game [ 0; 0; 0; 0; 0; 0 ] in
  let moves = Game_state.get_all_moves game in
  print_s [%sexp (moves : Move.t list)];
  [%expect
    {|
    (((column 1)) ((column 2)) ((column 3)) ((column 4)) ((column 5))
     ((column 6)))
    |}]
;;

let%expect_test "Player alternation" =
  let game = standard_game () in
  let game = Game_state.make_move game { column = 0 } |> ok_exn in
  (match game.decision with
   | In_progress { whose_turn } ->
     print_s [%sexp (whose_turn : Player_kind.t)];
     [%expect {| Yellow |}]
   | _ -> ());
  let game = Game_state.make_move game { column = 1 } |> ok_exn in
  match game.decision with
  | In_progress { whose_turn } ->
    print_s [%sexp (whose_turn : Player_kind.t)];
    [%expect {| Red |}]
  | _ -> ()
;;

let%expect_test "Get cell returns correct player" =
  let game = standard_game () in
  let game = Game_state.make_move game { column = 3 } |> ok_exn in
  let cell = Game_state.get_cell game { row = 5; column = 3 } in
  print_s [%sexp (cell : Player_kind.t option)];
  [%expect {| (Red) |}];
  let empty_cell = Game_state.get_cell game { row = 0; column = 0 } in
  print_s [%sexp (empty_cell : Player_kind.t option)];
  [%expect {| () |}]
;;

let%expect_test "Find lowest empty row in column" =
  let game = standard_game () in
  let row = Game_state.For_testing.find_lowest_empty_row game 0 in
  print_s [%sexp (row : int option)];
  [%expect {| (5) |}];
  let game = Game_state.make_move game { column = 0 } |> ok_exn in
  let row = Game_state.For_testing.find_lowest_empty_row game 0 in
  print_s [%sexp (row : int option)];
  [%expect {| (4) |}]
;;

let%expect_test "All directions for win checking" =
  print_s [%sexp (Game_state.For_testing.all_directions : (int * int) list)];
  [%expect {| ((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) |}]
;;

let%expect_test "Board full detection" =
  let game = standard_game () in
  let is_full = Game_state.is_board_full game in
  print_s [%sexp (is_full : bool)];
  [%expect {| false |}];
  (* Fill two columns completely *)
  let game = make_moves game [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1 ] in
  let is_full = Game_state.is_board_full game in
  print_s [%sexp (is_full : bool)];
  [%expect {| false |}]
;;

(* AI Tests *)

let%expect_test "AI can suggest a move from initial position" =
  let game = standard_game () in
  let move = AI.get_best_move game ~depth:3 in
  print_s [%sexp (move : Move.t option)];
  [%expect {| (((column 3))) |}]
;;

let%expect_test "AI blocks opponent's winning move" =
  (* Set up a position where Yellow has 3 in a row and can win *)
  let game = standard_game () in
  let game = make_moves game [ 0; 1; 0; 2; 5 ] in
  (* Yellow has pieces in columns 1, 2 and needs column 3 to win *)
  (* Red's turn - AI should block by playing column 3 *)
  let move = AI.get_best_move game ~depth:4 in
  print_s [%sexp (move : Move.t option)];
  [%expect {| (((column 3))) |}]
;;

let%expect_test "AI takes winning move when available" =
  let game = standard_game () in
  let game = make_moves game [ 0; 4; 1; 5; 2 ] in
  let move = AI.get_best_move game ~depth:4 in
  print_s [%sexp (move : Move.t option)];
  [%expect {| (((column 3))) |}]
;;

let%expect_test "AI returns None for finished game" =
  let game = standard_game () in
  let game = make_moves game [ 0; 4; 1; 5; 2; 6; 3 ] in
  (* Red has won *)
  let move = AI.get_best_move game ~depth:4 in
  print_s [%sexp (move : Move.t option)];
  [%expect {| () |}]
;;
