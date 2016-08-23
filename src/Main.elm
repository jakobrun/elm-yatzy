import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Dice
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
  , score: Int
  , values: Dict String Int
  }

type alias Player =
  { name : String
  }

type alias Model =
  { dices : List Dice.Model
  , players: List Player
  , upperScoreBoxes: List ScoreBox
  , lowerScoreBoxes: List ScoreBox
  , activePlayer: String
  , rollesLeft: Int
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
    [ Dice.fromValue 1 1
    , Dice.fromValue 1 2
    , Dice.fromValue 1 3
    , Dice.fromValue 1 4
    , Dice.fromValue 1 5
    ]
    [ Player "Tina"
    , Player "MJ"
    ]
    [ ScoreBox (SameNumber 1) 0 Dict.empty
    , ScoreBox (SameNumber 2) 0 Dict.empty
    , ScoreBox (SameNumber 3) 0 Dict.empty
    , ScoreBox (SameNumber 4) 0 Dict.empty
    , ScoreBox (SameNumber 5) 0 Dict.empty
    , ScoreBox (SameNumber 6) 0 Dict.empty
    ]
    [ ScoreBox (OfAKind 2) 0 Dict.empty
    , ScoreBox TwoPairs 0 Dict.empty
    , ScoreBox (OfAKind 3) 0 Dict.empty
    , ScoreBox (OfAKind 4) 0 Dict.empty
    , ScoreBox SmallStright 0 Dict.empty
    , ScoreBox LargeStright 0 Dict.empty
    , ScoreBox FullHouse 0 Dict.empty
    , ScoreBox Chance 0 Dict.empty
    , ScoreBox Yatzy 0 Dict.empty
    ]
    "Tina"
    3
    , Cmd.none)

-- UPDATE

type Msg
  = RollDices
  | DiceResult (List Int)
  | DiceMsg Dice.Msg
  | SelectScoreBox ScoreBox

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RollDices ->
      (model, model.dices |> List.length |> throwNDices)
    DiceResult res ->
      ({model |
        dices = List.map2 Dice.setValue model.dices res
      , rollesLeft = model.rollesLeft - 1
      } |> calulateScoreBoxes, Cmd.none)
    DiceMsg dMsg ->
        ({model | dices = List.map (Dice.update dMsg) model.dices}, Cmd.none)
    SelectScoreBox box ->
      (selectScoreBox box model |> moveToNextPlayer, Cmd.none)

throwNDices: Int -> Cmd Msg
throwNDices n =
  Random.generate DiceResult (Random.list n (Random.int 1 6))

selectScoreBox box model =
  {model |
    upperScoreBoxes =
      List.map (updateScoreBox box model) model.upperScoreBoxes,
    lowerScoreBoxes =
      List.map (updateScoreBox box model) model.lowerScoreBoxes
  }

calulateScoreBoxes model =
  {model
  | upperScoreBoxes = List.map (calculateScoreBox model.dices) model.upperScoreBoxes
  , lowerScoreBoxes = List.map (calculateScoreBox model.dices) model.lowerScoreBoxes
  }
calculateScoreBox dices box =
  {box | score =
    (case box.boxType of
      SameNumber value -> calculateSameNumber value dices
      OfAKind value -> calculateXOfAKind value dices
      TwoPairs -> calculateTwoPairs dices
      FullHouse -> calculateFullHouse dices
      SmallStright -> calculateSmallStright dices
      LargeStright -> calculateLargeStright dices
      Chance -> calculateChange dices
      Yatzy -> calculateYatzy dices
    )
  }

moveToNextPlayer model =
  {model |
    rollesLeft = 3,
    dices = List.map (\d -> {d | loose = True}) model.dices,
    activePlayer =
      case nextPlayer model.activePlayer model.players of
        Just player -> player.name
        Nothing ->
          case List.head model.players of
            Just player -> player.name
            Nothing -> model.activePlayer
  }

nextPlayer name players =
  let
    tailPlayers = case List.tail players of
      Just list -> list
      Nothing -> []
  in
    case List.head players of
      Just player ->
        if player.name == name then
          List.head tailPlayers
        else
          nextPlayer name tailPlayers
      Nothing -> Nothing

updateScoreBox box model oldBox =
  if box == oldBox then
    {box | values =
      Dict.insert
        model.activePlayer
        box.score
        box.values
    }
  else
    oldBox

calculateTotalScoreBoxForPlayer : List ScoreBox -> Player -> Int
calculateTotalScoreBoxForPlayer scoreBoxes player =
  List.filterMap (Dict.get player.name << .values) scoreBoxes
  |> List.sum

calculateBonusForPlayer model player =
  if (calculateTotalScoreBoxForPlayer model.upperScoreBoxes player) >= 63 then
    50
  else
    0

calculateTotalForPlayer model player =
  (calculateTotalScoreBoxForPlayer model.upperScoreBoxes player) +
  (calculateBonusForPlayer model player) +
  (calculateTotalScoreBoxForPlayer model.lowerScoreBoxes player)

calculateSameNumber value dices =
  List.map .value dices
  |> List.filter ((==) value)
  |> List.sum

calculateXOfAKind value dices =
  case
    (countSameNumer dices
      |> List.filter ((<=) value << snd)
      |> List.map ((*) value << fst)
      |> List.maximum
    ) of
      Nothing -> 0
      Just v -> v

calculateTwoPairs dices =
  let pairs =
    countSameNumer dices
      |> List.filter ((<=) 2 << snd)
  in
    if List.length pairs == 2 then
      List.map ((*) 2 << fst) pairs |> List.sum
    else
      0

calculateSmallStright dices =
  let faceCount =
    countSameNumer dices
  in
    if (List.length faceCount) == 5 && not (List.member 6 (List.map fst faceCount))
    then 15 else 0

calculateLargeStright dices =
  let faceCount =
    countSameNumer dices
  in
    if (List.length faceCount) == 5 && not (List.member 1 (List.map fst faceCount))
    then 20 else 0

calculateFullHouse dices =
  let faceCount =
    countSameNumer dices
  in
    if List.length faceCount == 2 &&
        (case List.head faceCount of
                Nothing -> 0
                Just v -> snd v
            ) >= 2
    then
      List.map .value dices |> List.sum
    else
      0

calculateChange = List.sum << List.map .value

calculateYatzy dices =
  if countSameNumer dices |> List.length |> (==) 1 then 50 else 0

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
        ++ List.map (viewScoreBox model) model.upperScoreBoxes ++
        [ viewTotalRow "Sum"
            (calculateTotalScoreBoxForPlayer model.upperScoreBoxes)
            model
        , viewTotalRow "Bonus"
            (calculateBonusForPlayer model)
            model
        ]
        ++ List.map (viewScoreBox model) model.lowerScoreBoxes ++
        [ viewTotalRow "Total"
            (calculateTotalForPlayer model)
            model
        ]
        )
      ]
    , div [class "flex"] (List.map (viewDice model) model.dices)
    , button [ onClick RollDices, disabled (model.rollesLeft == 0)] [ text "Roll"]
    , span []
      [text (model.activePlayer ++
        " has " ++
        toString model.rollesLeft ++
        " rolls left")
      ]
    ]

viewDice model dice =
  Html.App.map (DiceMsg) (Dice.view (model.rollesLeft == 3) dice)

viewUser player = th [class "border"] [text player.name]

viewScoreBox model scoreBox =
  tr []
    (
      [td [class "border"]
        [ text (scoreBoxLabel scoreBox.boxType)
        ]
      ]
      ++ List.map (viewScoreValue model scoreBox) model.players
    )

viewScoreValue model box player =
  td
    [class "border"]
    [
        (case Dict.get player.name box.values of
          Nothing ->
            if player.name == model.activePlayer && model.rollesLeft /= 3 then
              button [onClick (SelectScoreBox box)] [toString box.score |> text]
            else
              text ""
          Just value -> toString value |> text)
    ]

viewTotalRow label calcFun model =
  tr []
    ([th [class "border"] [text label]] ++
      List.map
        (viewScoreTotalValue calcFun)
        model.players
    )

viewScoreTotalValue calc player =
  th
    [class "border"]
    [text (toString(calc player))]
