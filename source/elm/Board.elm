module Board exposing
    ( Board
    , addFault
    , addX
    , faultPoints
    , gameEnded
    , getFaults
    , getRow
    , init
    , points
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


getRow : Color -> Board -> Row
getRow color (Board board) =
    Colors.get color board.rows


getFaults : Board -> Int
getFaults (Board board) =
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


points : Board -> Int
points board =
    (Color.all
        |> List.map (\color -> Row.points (Color.growth color) (getRow color board))
        |> List.foldl (+) 0
    )
        - faultPoints board


faultPoints : Board -> Int
faultPoints board =
    getFaults board * 5


gameEnded : Board -> Bool
gameEnded ((Board b) as board) =
    let
        closedRowsCount =
            Color.all
                |> List.map (\color -> Row.getLock (Color.growth color) (getRow color board))
                |> List.filter identity
                |> List.length
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
