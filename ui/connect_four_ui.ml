open! Core
open Tictactoe_logic_library
open Connect_four_logic
open Virtual_dom
open! Bonsai.Let_syntax

let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

(* Red circular piece *)
let red_piece =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='40' fill='#E74C3C' stroke='#C0392B' stroke-width='2' />"
    ()
;;

(* Yellow circular piece *)
let yellow_piece =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='40' fill='#F1C40F' stroke='#F39C12' stroke-width='2' />"
    ()
;;

(* Game status display *)
let status_display (game_state : Game_state.t) =
  let message =
    match game_state.decision with
    | Decision.In_progress { whose_turn } ->
      (match whose_turn with
       | Player_kind.Red -> "Red's Turn"
       | Player_kind.Yellow -> "Yellow's Turn")
    | Decision.Winner player ->
      (match player with
       | Player_kind.Red -> "🎉 Red Wins!"
       | Player_kind.Yellow -> "🎉 Yellow Wins!")
    | Decision.Stalemate -> "Draw!"
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "status" ]
    [ Vdom.Node.text message ]
;;

(* New game button *)
let new_game_button ~on_click =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.class_ "new-game-button"
      ; Vdom.Attr.on_click (fun _ -> on_click ())
      ]
    [ Vdom.Node.text "New Game" ]
;;

(* Render the Connect Four board *)
let connect_four_board ~(game_state : Game_state.t) ~set_game_state =
  let is_game_over = Decision.is_game_over game_state.decision in

  let render_cell ~row ~column =
    let cell_value = Game_state.get_cell game_state { Cell_position.row; column } in
    let mark =
      match cell_value with
      | Some Player_kind.Red -> red_piece
      | Some Player_kind.Yellow -> yellow_piece
      | None -> Vdom.Node.none
    in
    let should_slowly_appear =
      match game_state.last_move with
      | None -> []
      | Some last_move ->
        if last_move.row = row && last_move.column = column
        then [ Vdom.Attr.class_ "slowly_appear" ]
        else []
    in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.class_ "cell"
        ; Vdom.Attr.style
            Css_gen.(
              left (`Percent (Percent.of_percentage (Int.to_float column *. 100.0 /. 7.0)))
              @> width (`Percent (Percent.of_percentage (100.0 /. 7.0)))
              @> top (`Percent (Percent.of_percentage (Int.to_float row *. 100.0 /. 6.0)))
              @> height (`Percent (Percent.of_percentage (100.0 /. 6.0))))
        ]
      [ Vdom.Node.div
          ~attrs:(should_slowly_appear @ [ Vdom.Attr.class_ "piece" ])
          [ mark ]
      ]
  in

  let render_column_button column =
    let can_play =
      (not is_game_over)
      && (match Game_state.For_testing.find_lowest_empty_row game_state column with
         | Some _ -> true
         | None -> false)
    in
    let button_text = Int.to_string column in
    let maybe_clickable_attr =
      if can_play
      then
        Vdom.Attr.on_click (fun _ ->
          match Game_state.make_move game_state { Move.column } with
          | Error _ -> Vdom.Effect.Ignore
          | Ok new_game_state -> set_game_state new_game_state)
      else Vdom.Attr.create "disabled" "disabled"
    in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ "column-button"
        ; maybe_clickable_attr
        ]
      [ Vdom.Node.text button_text ]
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "board-container" ]
    [ (* Column buttons above the board *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "column-buttons" ]
        (List.init 7 ~f:render_column_button)
    ; (* The game board *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "board" ]
        (List.concat_map (List.init 6 ~f:Fn.id) ~f:(fun row ->
           List.init 7 ~f:(fun column -> render_cell ~row ~column)))
    ]
;;

(* Main app component *)
let app =
  let initial_state = Game_state.create_standard () in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game-container" ]
    [ Vdom.Node.h1 ~attrs:[] [ Vdom.Node.text "Connect Four" ]
    ; status_display game_state
    ; connect_four_board ~game_state ~set_game_state
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "controls" ]
        [ new_game_button ~on_click:(fun () -> set_game_state initial_state) ]
    ]
;;

let () = Bonsai_web.Start.start app
