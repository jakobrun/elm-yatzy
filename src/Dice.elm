module Dice exposing (Model, fromValue, view, Msg, update, setValue)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
  { value: Int
  , id: Int
  , loose: Bool
  }
type Msg
  = Toggle Int

fromValue value id = Model value id True

update: Msg -> Model -> Model
update msg dice =
  case msg of
    Toggle id -> toggleDice id dice

setValue dice newValue =
    if dice.loose then {dice | value = newValue} else dice

toggleDice: Int -> Model -> Model
toggleDice id dice =
  if dice.id == id then {dice | loose = not dice.loose} else dice


view : Bool -> Model -> Html Msg
view diceDisabled dice =
  button
    [ class ("block dice rounded " ++ diceStyle dice)
    , Toggle dice.id |> onClick
    , disabled diceDisabled
    ]
    [ viewDiceValue dice.value]

diceStyle: Model -> String
diceStyle dice = if dice.loose then "dice-loose" else "dice-pinned"

viewDiceValue value
  = case value of
    1 -> div [class "first-face"] (viewPip 1)
    2 -> div [class "second-face"] (viewPip 2)
    3 -> div [class "third-face"] (viewPip 3)
    4 -> div [class "fourth-face"]
          [ viewColumn 2
          , viewColumn 2
          ]
    5 -> div [class "fifth-face"]
          [ viewColumn 2
          , viewColumn 1
          , viewColumn 2
          ]
    6 -> div [class "sixth-face"]
          [ viewColumn 3
          , viewColumn 3
          ]
    _ -> div [] [text "wtf!!!"]

viewColumn n = div [class "column"] (viewPip n)

viewPip n = List.repeat n (span [class "pip"] [])
