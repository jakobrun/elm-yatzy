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
type ScoreBoxValue
  = Empty
  | Strike
  | Value Int

type ScoreBoxType
  = SameNumber Int
  | OfAKind Int
  | TwoPairs
  | SmallStright
  | LargeStright
  | Chance
  | Yatzy

type alias ScoreBox =
  { boxType: ScoreBoxType
  , values: List ScoreBoxValue
  }

type alias Player =
  { name : String
  }

type alias Dice =
  { value: Int
  , id: Int
  , loose: Bool
  }

type alias Model =
  { dices : List Dice
  , players: List Player
  , upperScoreBoxes: List ScoreBox
  , upperTotals: List ScoreBoxValue
  , bonus: List ScoreBoxValue
  , lowerScoreBoxes: List ScoreBox
  , totals: List ScoreBoxValue
  }

init : (Model, Cmd Msg)
init =
  (Model
    [ Dice 1 1 True
    , Dice 1 2 True
    , Dice 1 3 True
    , Dice 1 4 True
    , Dice 1 5 True
    ]
    [ Player "Tina"
    , Player "MJ"
    ]
    [ ScoreBox (SameNumber 1) [Empty, Value 4]
    , ScoreBox (SameNumber 2) [Empty, Empty]
    , ScoreBox (SameNumber 3) [Empty, Empty]
    , ScoreBox (SameNumber 4) [Empty, Empty]
    , ScoreBox (SameNumber 5) [Empty, Empty]
    , ScoreBox (SameNumber 6) [Empty, Empty]
    ]
    [ Empty
    , Empty
    ]
    [ Empty
    , Empty
    ]
    [ ScoreBox (OfAKind 2) [Strike, Empty]
    , ScoreBox TwoPairs [Strike, Empty]
    , ScoreBox (OfAKind 3) [Strike, Empty]
    , ScoreBox (OfAKind 4) [Strike, Empty]
    , ScoreBox SmallStright [Empty, Value 10]
    , ScoreBox LargeStright [Empty, Value 10]
    , ScoreBox Chance [Empty, Value 10]
    , ScoreBox Yatzy [Empty, Value 10]
    ]
    [ Empty
    , Empty
    ]
    , Cmd.none)

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
    [ h1 [] [text "YATZY"]
    , table [class "p0"]
      ([ tr []
        ([th [class "border"] [text ""]] ++ List.map viewUser model.players)
      ]
      ++ List.map viewScoreBox model.upperScoreBoxes ++
      [ tr []
        ([th [class "border"] [text "Sum"]] ++ List.map viewScoreValue model.upperTotals)
      , tr []
        ([th [class "border"] [text "Bonus"]] ++ List.map viewScoreValue model.bonus)
      ]
      ++ List.map viewScoreBox model.lowerScoreBoxes ++
      [ tr []
        ([th [class "border"] [text "Total"]] ++ List.map viewScoreValue model.upperTotals)
      ]
      )
    , div [] (List.map viewDice model.dices)
    , button [ onClick ThrowDices] [ text "Throw"]
    ]

viewUser player = th [class "border"] [text player.name]

viewScoreBox scoreBox =
  tr []
    ([td [class "border"] [text (toString scoreBox.boxType)]]
    ++ List.map viewScoreValue scoreBox.values)

viewScoreValue scoreBoxValue =
  td [class "border"] [scoreBoxValue |> scoreBoxValueToString |> text]

scoreBoxValueToString scoreBoxValue =
  case scoreBoxValue of
    Empty -> ""
    Strike -> "-"
    Value v -> toString v

viewDice : Dice -> Html Msg
viewDice dice =
  button
    [ class ("border inline-block rounded p2 m1 bg-white" ++ (diceStyle dice))
    , ToggleDice dice.id |> onClick
    ]
    [ text (toString dice.value)]

diceStyle: Dice -> String
diceStyle dice = if dice.loose then "" else " bc-red"
