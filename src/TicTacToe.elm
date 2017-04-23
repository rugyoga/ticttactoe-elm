module Chess.TicTacToe exposing (..)

import Html exposing (Html, button, div, text, table, td, tr)
import Html.Events exposing (onClick)
import Dict exposing (Dict, empty, get, insert, size)
import List exposing (all, any, head, map, range)

type alias Coordinate = (Int, Int)
type Counter = X | O
type Result = Win Counter | Draw
type alias Msg = Coordinate
type alias Board = Dict Coordinate Counter
type alias Moves = List Coordinate
type alias Model = { board : Board, to_move : Counter, moves : Moves, result : Maybe Result }

initial_model : Model
initial_model = { board = empty, to_move = X, moves = [], result = Nothing }

counter_to_string : Counter -> String
counter_to_string c = if c == X then "X" else "O"

board_to_table : Board -> Html Msg
board_to_table board =
  let row_to_tr y = tr [] (map (square_to_td y) (range 0 2))
      square_to_td y x =
        let t = case get (x, y) board of
          Just c  -> text (counter_to_string c)
          Nothing -> button [onClick (x, y)] [text "."] in
        td [] [t] in
  table [] (map row_to_tr (range 0 2))

checks : Coordinate -> List (List Coordinate)
checks (x,y) =
  [[(x,0), (x,1), (x,2)], [(0,y), (1,y), (2,y)]] ++
  (if x == y     then [[(0,0), (1,1), (2, 2)]] else []) ++
  (if x + y == 2 then [[(0,2), (1,1), (2, 0)]] else [])

result_to_string : Result -> String
result_to_string result =
  case result of
    Draw  -> "draw"
    Win c -> (counter_to_string c) ++ " wins!"

view : Model -> Html Msg
view model =
  div []
  [board_to_table model.board,
  case model.result of
    Just value -> text (result_to_string value)
    Nothing    -> text ((counter_to_string model.to_move) ++ " to move")]

result : Coordinate -> Board -> Counter -> Maybe Result
result move board to_move =
  if any (all (\sq -> get sq board == Just to_move)) (checks move)
  then Just (Win to_move)
  else if size board == 9
       then Just Draw
       else Nothing

update : Msg -> Model -> Model
update move model =
  let board2 = insert move model.to_move model.board in
  { to_move = if model.to_move == X then O else X,
    board   = board2,
    moves   = move :: model.moves,
    result  = result move board2 model.to_move }

main = Html.beginnerProgram { model = initial_model, view = view, update = update }
