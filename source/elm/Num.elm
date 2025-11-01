module Num exposing
    ( Num(..)
    , allBackward
    , allForward
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


allForward : List Num
allForward =
    [ Num2
    , Num3
    , Num4
    , Num5
    , Num6
    , Num7
    , Num8
    , Num9
    , Num10
    , Num11
    , Num12
    ]


allBackward : List Num
allBackward =
    [ Num12
    , Num11
    , Num10
    , Num9
    , Num8
    , Num7
    , Num6
    , Num5
    , Num4
    , Num3
    , Num2
    ]


isLast : Bool -> Num -> Bool
isLast reverse num =
    getLast reverse == num


getLast : Bool -> Num
getLast reverse =
    if reverse then
        Num2

    else
        Num12


next : Bool -> Num -> Maybe Num
next reverse num =
    case ( reverse, num ) of
        ( False, Num2 ) ->
            Just Num3

        ( False, Num3 ) ->
            Just Num4

        ( False, Num4 ) ->
            Just Num5

        ( False, Num5 ) ->
            Just Num6

        ( False, Num6 ) ->
            Just Num7

        ( False, Num7 ) ->
            Just Num8

        ( False, Num8 ) ->
            Just Num9

        ( False, Num9 ) ->
            Just Num10

        ( False, Num10 ) ->
            Just Num11

        ( False, Num11 ) ->
            Just Num12

        ( False, Num12 ) ->
            Nothing

        ( True, Num2 ) ->
            Nothing

        ( True, Num3 ) ->
            Just Num2

        ( True, Num4 ) ->
            Just Num3

        ( True, Num5 ) ->
            Just Num4

        ( True, Num6 ) ->
            Just Num5

        ( True, Num7 ) ->
            Just Num6

        ( True, Num8 ) ->
            Just Num7

        ( True, Num9 ) ->
            Just Num8

        ( True, Num10 ) ->
            Just Num9

        ( True, Num11 ) ->
            Just Num10

        ( True, Num12 ) ->
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
