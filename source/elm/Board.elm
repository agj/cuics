module Board exposing
    ( Board
    , addFault
    , addX
    , faultPoints
    , faults
    , gameEnded
    , init
    , lockedRows
    , points
    , row
    , updateRow
    )

import Color exposing (Color(..))
import Colors exposing (Colors)
import Num exposing (Num)
import Row exposing (Row)


type Board
    = Board
        { rows : Colors Row
        , faults : FaultsCount
        }


type FaultsCount
    = Faults0
    | Faults1
    | Faults2
    | Faults3
    | Faults4


init : Board
init =
    Board
        { rows = Colors.init Row.init
        , faults = Faults0
        }


row : Color -> Board -> Row
row color (Board board) =
    Colors.get color board.rows


faults : Board -> Int
faults (Board board) =
    case board.faults of
        Faults0 ->
            0

        Faults1 ->
            1

        Faults2 ->
            2

        Faults3 ->
            3

        Faults4 ->
            4


lockedRows : Board -> List Color
lockedRows board =
    Color.all
        |> List.filter
            (\color ->
                row color board
                    |> Row.locked (Color.growth color)
            )


points : Board -> Int
points board =
    (Color.all
        |> List.map (\color -> Row.points (Color.growth color) (row color board))
        |> List.foldl (+) 0
    )
        - faultPoints board


faultPoints : Board -> Int
faultPoints board =
    faults board * 5


gameEnded : Board -> Bool
gameEnded ((Board b) as board) =
    let
        closedRowsCount =
            List.length (lockedRows board)
    in
    (closedRowsCount >= 2)
        || ((b.faults |> Debug.log "faults") == Faults4)


updateRow : Color -> (Row -> Row) -> Board -> Board
updateRow color rowUpdater (Board board) =
    Board { board | rows = Colors.update color rowUpdater board.rows }


addX : Color -> Num -> Board -> Board
addX color num board =
    updateRow color (Row.set num True) board


addFault : Board -> Board
addFault (Board board) =
    let
        nextFaults =
            case board.faults of
                Faults0 ->
                    Faults1

                Faults1 ->
                    Faults2

                Faults2 ->
                    Faults3

                Faults3 ->
                    Faults4

                Faults4 ->
                    Faults4
    in
    Board { board | faults = nextFaults }
