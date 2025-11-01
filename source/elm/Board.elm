module Board exposing (Board, addFault, getFaults, getRow, init, updateRow)

import Color exposing (Color(..))
import Row exposing (Row)


type Board
    = Board
        { redRow : Row Bool
        , yellowRow : Row Bool
        , greenRow : Row Bool
        , blueRow : Row Bool
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
        { redRow = Row.init False
        , yellowRow = Row.init False
        , greenRow = Row.init False
        , blueRow = Row.init False
        , faults = 0
        }


getRow : Color -> Board -> Row Bool
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


updateRow : Color -> (Row Bool -> Row Bool) -> Board -> Board
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


addFault : Board -> Board
addFault (Board board) =
    Board { board | faults = board.faults + 1 }
