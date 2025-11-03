module Num exposing
    ( Growth(..)
    , Num(..)
    , all
    , getLast
    , isLast
    , next
    , toInt
    )


type Num
    = Num2
    | Num3
    | Num4
    | Num5
    | Num6
    | Num7
    | Num8
    | Num9
    | Num10
    | Num11
    | Num12


type Growth
    = Grows
    | Shrinks


all : Growth -> List Num
all growth =
    case growth of
        Grows ->
            allGrowing

        Shrinks ->
            allShrinking


allGrowing : List Num
allGrowing =
    [ Num2, Num3, Num4, Num5, Num6, Num7, Num8, Num9, Num10, Num11, Num12 ]


allShrinking : List Num
allShrinking =
    [ Num12, Num11, Num10, Num9, Num8, Num7, Num6, Num5, Num4, Num3, Num2 ]


isLast : Growth -> Num -> Bool
isLast growth num =
    getLast growth == num


getLast : Growth -> Num
getLast growth =
    case growth of
        Grows ->
            Num12

        Shrinks ->
            Num2


next : Growth -> Num -> Maybe Num
next growth num =
    case ( growth, num ) of
        ( Grows, Num2 ) ->
            Just Num3

        ( Grows, Num3 ) ->
            Just Num4

        ( Grows, Num4 ) ->
            Just Num5

        ( Grows, Num5 ) ->
            Just Num6

        ( Grows, Num6 ) ->
            Just Num7

        ( Grows, Num7 ) ->
            Just Num8

        ( Grows, Num8 ) ->
            Just Num9

        ( Grows, Num9 ) ->
            Just Num10

        ( Grows, Num10 ) ->
            Just Num11

        ( Grows, Num11 ) ->
            Just Num12

        ( Grows, Num12 ) ->
            Nothing

        ( Shrinks, Num2 ) ->
            Nothing

        ( Shrinks, Num3 ) ->
            Just Num2

        ( Shrinks, Num4 ) ->
            Just Num3

        ( Shrinks, Num5 ) ->
            Just Num4

        ( Shrinks, Num6 ) ->
            Just Num5

        ( Shrinks, Num7 ) ->
            Just Num6

        ( Shrinks, Num8 ) ->
            Just Num7

        ( Shrinks, Num9 ) ->
            Just Num8

        ( Shrinks, Num10 ) ->
            Just Num9

        ( Shrinks, Num11 ) ->
            Just Num10

        ( Shrinks, Num12 ) ->
            Just Num11


toInt : Num -> Int
toInt num =
    case num of
        Num2 ->
            2

        Num3 ->
            3

        Num4 ->
            4

        Num5 ->
            5

        Num6 ->
            6

        Num7 ->
            7

        Num8 ->
            8

        Num9 ->
            9

        Num10 ->
            10

        Num11 ->
            11

        Num12 ->
            12
