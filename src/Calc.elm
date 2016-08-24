module Calc exposing
  ( sameNumber
  , xOfAKind
  , twoPairs
  , smallStright
  , largeStright
  , fullHouse
  , chance
  , yatzy
  )

countSameNumer =
  List.foldr
    (\value newList ->
      if List.map fst newList |> List.member value then
        List.map (\pair -> if fst pair == value then (value, (snd pair) + 1) else pair) newList
      else
        (value, 1) :: newList
    )
    []

sameNumber: Int -> List Int -> Int
sameNumber value = List.sum << List.filter ((==) value)

xOfAKind: Int -> List Int -> Int
xOfAKind value =
  zeroOrValue <<
    (
      List.maximum <<
      List.map ((*) value << fst) <<
      List.filter ((<=) value << snd) <<
      countSameNumer
    )

zeroOrValue maybeValue = case maybeValue of
  Nothing -> 0
  Just v -> v

twoPairs: List Int -> Int
twoPairs = sumIfTwoPairs << List.filter ((<=) 2 << snd) << countSameNumer

sumIfTwoPairs pairs =
  if List.length pairs == 2 then
    List.map ((*) 2 << fst) pairs |> List.sum
  else
    0


smallStright: List Int -> Int
smallStright = stright 6 15 << countSameNumer

largeStright: List Int -> Int
largeStright = stright 1 20 << countSameNumer

stright notMember score faceCount =
  if (List.length faceCount) == 5 && not (hasMember notMember faceCount)
  then score else 0

hasMember m = List.member m << List.map fst

fullHouse: List Int -> Int
fullHouse = fullHouseFromFaceCount << countSameNumer

fullHouseFromFaceCount faceCount =
  if List.length faceCount == 2 &&
      (case List.head faceCount of
              Nothing -> 0
              Just v -> snd v
          ) >= 2
  then
    List.map (\f -> fst f * snd f) faceCount |> List.sum
  else
    0

chance: List Int -> Int
chance = List.sum

yatzy: List Int -> Int
yatzy dicesValues =
  if countSameNumer dicesValues |> List.length |> (==) 1 then 50 else 0
