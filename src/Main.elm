import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Player =
  { name : String }

type alias Dice =
  { value: Int
  , id: Int
  , loose: Bool
  }

type alias Model =
  { dices : List Dice }

init : (Model, Cmd Msg)
init =
  (Model [Dice 1 1 True
    , Dice 1 2 True
    , Dice 1 3 False
    , Dice 1 4 True
    , Dice 1 5 True
    ], Cmd.none)

-- UPDATE

type Msg
  = ThrowDices
  | DiceResult (List Int)
  | ToggleDice Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ThrowDices ->
      (model, model.dices |> List.length |> throwNDices)
    DiceResult res ->
      ({model | dices = List.map2 diceRes model.dices res}, Cmd.none)
    ToggleDice id ->
      ({model | dices = List.map (toggleDice id) model.dices}, Cmd.none)

throwNDices: Int -> Cmd Msg
throwNDices n =
  Random.generate DiceResult (Random.list n (Random.int 1 6))

diceRes: Dice -> Int -> Dice
diceRes dice newValue =
  if dice.loose then {dice | value = newValue} else dice

toggleDice: Int -> Dice -> Dice
toggleDice id dice =
  if dice.id == id then {dice | loose = not dice.loose} else dice

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Zatzy"]
    , div [] (List.map viewDice model.dices)
    , button [ onClick ThrowDices] [ text "Throw"]
    ]

viewDice : Dice -> Html Msg
viewDice dice =
  button
    [ class ("border inline-block rounded p2 m1 bg-white" ++ (diceStyle dice))
    , ToggleDice dice.id |> onClick
    ]
    [ text (toString dice.value)]

diceStyle: Dice -> String
diceStyle dice = if dice.loose then "" else " bc-red"
