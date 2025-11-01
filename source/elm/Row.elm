module Row exposing (Row, count, get, init, set)

import Num exposing (Num(..), allForward)


type Row a
    = Row
        { num2 : a
        , num3 : a
        , num4 : a
        , num5 : a
        , num6 : a
        , num7 : a
        , num8 : a
        , num9 : a
        , num10 : a
        , num11 : a
        , num12 : a
        }


init : a -> Row a
init value =
    Row
        { num2 = value
        , num3 = value
        , num4 = value
        , num5 = value
        , num6 = value
        , num7 = value
        , num8 = value
        , num9 = value
        , num10 = value
        , num11 = value
        , num12 = value
        }


get : Num -> Row a -> a
get num (Row row) =
    case num of
        Num2 ->
            row.num2

        Num3 ->
            row.num3

        Num4 ->
            row.num4

        Num5 ->
            row.num5

        Num6 ->
            row.num6

        Num7 ->
            row.num7

        Num8 ->
            row.num8

        Num9 ->
            row.num9

        Num10 ->
            row.num10

        Num11 ->
            row.num11

        Num12 ->
            row.num12


set : Num -> a -> Row a -> Row a
set num value (Row row) =
    case num of
        Num2 ->
            Row { row | num2 = value }

        Num3 ->
            Row { row | num3 = value }

        Num4 ->
            Row { row | num4 = value }

        Num5 ->
            Row { row | num5 = value }

        Num6 ->
            Row { row | num6 = value }

        Num7 ->
            Row { row | num7 = value }

        Num8 ->
            Row { row | num8 = value }

        Num9 ->
            Row { row | num9 = value }

        Num10 ->
            Row { row | num10 = value }

        Num11 ->
            Row { row | num11 = value }

        Num12 ->
            Row { row | num12 = value }


count : (a -> Bool) -> Row a -> Int
count checker row =
    allForward
        |> List.map (\num -> get num row)
        |> List.filter checker
        |> List.length
