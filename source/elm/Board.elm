module Board exposing
    ( Board
    , addFault
    , addX
    , faultPoints
    , getFaults
    , getRow
    , init
    , points
    , updateRow
    )

import Color exposing (Color(..))
import Num exposing (Num)
import Row exposing (Row)


type Board
    = Board
        { redRow : Row
        , yellowRow : Row
        , greenRow : Row
        , blueRow : Row
        , faults : Int
        }


type FaultsCount
    = Faults1
    | Faults2
    | Faults3
    | Faults4


init : Board
init =
    Board
        { redRow = Row.init
        , yellowRow = Row.init
        , greenRow = Row.init
        , blueRow = Row.init
        , faults = 0
        }


getRow : Color -> Board -> Row
getRow color (Board board) =
    case color of
        Red ->
            board.redRow

        Yellow ->
            board.yellowRow

        Green ->
            board.greenRow

        Blue ->
            board.blueRow


getFaults : Board -> Int
getFaults (Board board) =
    board.faults


points : Board -> Int
points board =
    (Color.all
        |> List.map (\color -> Row.points (Color.isReverse color) (getRow color board))
        |> List.foldl (+) 0
    )
        - faultPoints board


faultPoints : Board -> Int
faultPoints board =
    getFaults board * 5


updateRow : Color -> (Row -> Row) -> Board -> Board
updateRow color rowUpdater (Board board) =
    case color of
        Red ->
            Board { board | redRow = rowUpdater board.redRow }

        Yellow ->
            Board { board | yellowRow = rowUpdater board.yellowRow }

        Green ->
            Board { board | greenRow = rowUpdater board.greenRow }

        Blue ->
            Board { board | blueRow = rowUpdater board.blueRow }


addX : Color -> Num -> Board -> Board
addX color num board =
    updateRow color (Row.set num True) board


addFault : Board -> Board
addFault (Board board) =
    Board { board | faults = board.faults + 1 }
