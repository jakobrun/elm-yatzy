module Die exposing (Model, fromValue, view, Msg, update, setValue)

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
update msg die =
  case msg of
    Toggle id -> toggleLoose id die

setValue die newValue =
    if die.loose then {die | value = newValue} else die

toggleLoose: Int -> Model -> Model
toggleLoose id die =
  if die.id == id then {die | loose = not die.loose} else die


view : Bool -> Model -> Html Msg
view dieDisabled die =
  button
    [ class ("block die rounded " ++ dieStyle die)
    , Toggle die.id |> onClick
    , disabled dieDisabled
    ]
    [ viewValue die.value]

dieStyle die = if die.loose then "die-loose" else "die-pinned"

viewValue value
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
