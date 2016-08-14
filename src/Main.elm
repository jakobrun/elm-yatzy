import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Random

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type ScoreBoxType
  = SameNumber Int
  | OfAKind Int
  | TwoPairs
  | SmallStright
  | LargeStright
  | FullHouse
  | Chance
  | Yatzy

type alias ScoreBox =
  { boxType: ScoreBoxType
  , values: Dict String Int
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
  , upperTotals: Dict String Int
  , bonus: Dict String Int
  , lowerScoreBoxes: List ScoreBox
  , totals: Dict String Int
  , activePlayer: String
  }

scoreBoxLabel scoreBoxType =
  case scoreBoxType of
    SameNumber value ->
      case value of
        1 -> "Ones"
        2 -> "Twos"
        3 -> "Threes"
        4 -> "Fours"
        5 -> "Fives"
        6 -> "Sixes"
        _ -> "WTF!"
    OfAKind value ->
      case value of
        2 -> "1 pair"
        _ -> (toString value) ++ " of a kind"
    TwoPairs -> "2 pairs"
    SmallStright -> "Small stright"
    LargeStright -> "Large stright"
    FullHouse -> "Full house"
    _ -> toString scoreBoxType

countSameNumer dices =
  List.map .value dices
  |> List.foldr
    (\value newList ->
      if List.map fst newList |> List.member value then
        List.map (\pair -> if fst pair == value then (value, (snd pair) + 1) else pair) newList
      else
        (value, 1) :: newList
    )
    []

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
    [ ScoreBox (SameNumber 1) Dict.empty
    , ScoreBox (SameNumber 2) Dict.empty
    , ScoreBox (SameNumber 3) Dict.empty
    , ScoreBox (SameNumber 4) Dict.empty
    , ScoreBox (SameNumber 5) Dict.empty
    , ScoreBox (SameNumber 6) Dict.empty
    ]
    Dict.empty
    Dict.empty
    [ ScoreBox (OfAKind 2) Dict.empty
    , ScoreBox TwoPairs Dict.empty
    , ScoreBox (OfAKind 3) Dict.empty
    , ScoreBox (OfAKind 4) Dict.empty
    , ScoreBox SmallStright Dict.empty
    , ScoreBox LargeStright Dict.empty
    , ScoreBox FullHouse Dict.empty
    , ScoreBox Chance Dict.empty
    , ScoreBox Yatzy Dict.empty
    ]
    Dict.empty
    "Tina"
    , Cmd.none)

-- UPDATE

type Msg
  = RollDices
  | DiceResult (List Int)
  | ToggleDice Int
  | SelectScoreBox ScoreBox

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RollDices ->
      (model, model.dices |> List.length |> throwNDices)
    DiceResult res ->
      ({model | dices = List.map2 diceRes model.dices res}, Cmd.none)
    ToggleDice id ->
      ({model | dices = List.map (toggleDice id) model.dices}, Cmd.none)
    SelectScoreBox box ->
      (selectScoreBox box model, Cmd.none)

throwNDices: Int -> Cmd Msg
throwNDices n =
  Random.generate DiceResult (Random.list n (Random.int 1 6))

diceRes: Dice -> Int -> Dice
diceRes dice newValue =
  if dice.loose then {dice | value = newValue} else dice

toggleDice: Int -> Dice -> Dice
toggleDice id dice =
  if dice.id == id then {dice | loose = not dice.loose} else dice

selectScoreBox box model =
  case box.boxType of
    SameNumber value ->
      {model |
        upperScoreBoxes =
          List.map
            (updateScoreBox box model (calculateSameNumber value))
            model.upperScoreBoxes
      }
    OfAKind value ->
      {model |
        lowerScoreBoxes =
          List.map
            (updateScoreBox box model (calculateXOfAKind value))
            model.lowerScoreBoxes
      }
    _ -> model

updateScoreBox box model calculateNewValue oldBox =
  if box == oldBox then
    {box | values =
      Dict.insert
        model.activePlayer
        (calculateNewValue model.dices)
        box.values
    }
  else
    oldBox

calculateXOfAKind value dices =
  case
    (countSameNumer dices
      |> List.filter (\v -> value <= snd v)
      |> List.map (\v -> (fst v) * value)
      |> List.maximum
    ) of
      Nothing -> 0
      Just v -> v

calculateSameNumber value dices =
  List.map .value dices
  |> List.filter (\v -> v == value)
  |> List.sum


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [class "paper p2 m2 inline-block"]
      [ h1 [] [text "YATZY"]
      , table []
        ([ tr []
          ([th [class "border"] [text ""]] ++ List.map viewUser model.players)
        ]
        ++ List.map (viewScoreBox model.players) model.upperScoreBoxes ++
        [ tr []
          ([th [class "border"] [text "Sum"]] ++ List.map (viewScoreValue model.upperTotals) model.players)
        , tr []
          ([th [class "border"] [text "Bonus"]] ++ List.map (viewScoreValue model.bonus) model.players)
        ]
        ++ List.map (viewScoreBox model.players) model.lowerScoreBoxes ++
        [ tr []
          ([th [class "border"] [text "Total"]] ++ List.map (viewScoreValue model.upperTotals) model.players)
        ]
        )
      ]
    , div [class "flex"] (List.map viewDice model.dices)
    , button [ onClick RollDices] [ text "Roll"]
    ]

viewColumn n = div [class "column"] (viewPip n)

viewUser player = th [class "border"] [text player.name]

viewScoreBox players scoreBox =
  tr []
    (
      [td [class "border"]
        [ button
          [ class "scorebox-button"
          , onClick (SelectScoreBox scoreBox)
          ]
          [ text (scoreBoxLabel scoreBox.boxType)
          ]
        ]
      ]
      ++ List.map (viewScoreValue scoreBox.values) players
    )

viewScoreValue values player =
  td
    [class "border"]
    [text
        (case Dict.get player.name values of
          Nothing -> ""
          Just value -> toString value)
    ]

viewDice : Dice -> Html Msg
viewDice dice =
  button
    [ class ("dice rounded " ++ diceStyle dice)
    , ToggleDice dice.id |> onClick
    ]
    [ viewDiceValue dice.value]

diceStyle: Dice -> String
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

viewPip n = List.repeat n (span [class "pip"] [])
