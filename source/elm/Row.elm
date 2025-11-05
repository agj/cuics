module Row exposing (Row, get, init, locked, points, set, xCount)

import Array exposing (Array)
import Num exposing (Num(..))


type Row
    = Row
        { num2 : Bool
        , num3 : Bool
        , num4 : Bool
        , num5 : Bool
        , num6 : Bool
        , num7 : Bool
        , num8 : Bool
        , num9 : Bool
        , num10 : Bool
        , num11 : Bool
        , num12 : Bool
        }


init : Row
init =
    Row
        { num2 = False
        , num3 = False
        , num4 = False
        , num5 = False
        , num6 = False
        , num7 = False
        , num8 = False
        , num9 = False
        , num10 = False
        , num11 = False
        , num12 = False
        }


get : Num -> Row -> Bool
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


locked : Num.Growth -> Row -> Bool
locked growth row =
    get (Num.last growth) row


set : Num -> Bool -> Row -> Row
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


xCount : Num.Growth -> Row -> Int
xCount growth row =
    let
        baseCount =
            Num.all Num.Grows
                |> List.map (\num -> get num row)
                |> List.filter identity
                |> List.length

        lockBonus =
            if locked growth row then
                1

            else
                0
    in
    baseCount + lockBonus


points : Num.Growth -> Row -> Int
points growth row =
    pointsTable
        |> Array.get (xCount growth row - 1)
        |> Maybe.withDefault 0


pointsTable : Array number
pointsTable =
    [ 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78 ]
        |> Array.fromList
