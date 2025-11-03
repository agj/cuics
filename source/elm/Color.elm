module Color exposing (Color(..), all, growth)

import Num


type Color
    = Red
    | Yellow
    | Green
    | Blue


all : List Color
all =
    [ Red, Yellow, Green, Blue ]


growth : Color -> Num.Growth
growth color =
    case color of
        Red ->
            Num.Grows

        Yellow ->
            Num.Grows

        Green ->
            Num.Shrinks

        Blue ->
            Num.Shrinks
