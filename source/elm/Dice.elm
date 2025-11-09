module Dice exposing (Dice, Die(..), build, get, init)


type Dice a
    = Dice
        { white1 : a
        , white2 : a
        , red : a
        , yellow : a
        , green : a
        , blue : a
        }


type Die
    = White1
    | White2
    | Red
    | Yellow
    | Green
    | Blue


init : a -> Dice a
init value =
    Dice
        { white1 = value
        , white2 = value
        , red = value
        , yellow = value
        , green = value
        , blue = value
        }


build : a -> a -> a -> a -> a -> a -> Dice a
build white1 white2 red yellow green blue =
    Dice
        { white1 = white1
        , white2 = white2
        , red = red
        , yellow = yellow
        , green = green
        , blue = blue
        }


get : Die -> Dice a -> a
get die (Dice dice) =
    case die of
        White1 ->
            dice.white1

        White2 ->
            dice.white2

        Red ->
            dice.red

        Yellow ->
            dice.yellow

        Green ->
            dice.green

        Blue ->
            dice.blue
