module Color exposing (Color(..), all, isReverse)


type Color
    = Red
    | Yellow
    | Green
    | Blue


all : List Color
all =
    [ Red, Yellow, Green, Blue ]


isReverse : Color -> Bool
isReverse color =
    case color of
        Red ->
            False

        Yellow ->
            False

        Green ->
            True

        Blue ->
            True
