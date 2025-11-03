module Main exposing (main)

import Board exposing (Board)
import Browser
import Color exposing (Color(..))
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Events as Events
import List
import Num exposing (Num(..))
import Row exposing (Row)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Svga
import Tailwind.Theme as Twc
import Tailwind.Utilities as Tw
import Util.Html.Styled.Attributes exposing (attributeIf)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { board : Board
    , diceThrow : DiceThrow
    , turn : Turn
    }


type CellStatus
    = Available
    | Xed
    | Unavailable


type alias DiceThrow =
    { dieWhite1 : Pips
    , dieWhite2 : Pips
    , dieRed : Pips
    , dieYellow : Pips
    , dieGreen : Pips
    , dieBlue : Pips
    }


type Turn
    = NotTurn
    | TurnPickingWhite
    | TurnPickingColored



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.init
      , diceThrow =
            { dieWhite1 = Pips1
            , dieWhite2 = Pips2
            , dieRed = Pips3
            , dieYellow = Pips4
            , dieGreen = Pips5
            , dieBlue = Pips6
            }
      , turn = TurnPickingColored
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedCell Color Num
    | ClickedFault


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell color num ->
            ( { model | board = Board.updateRow color (Row.set num True) model.board }
            , Cmd.none
            )

        ClickedFault ->
            ( { model | board = Board.addFault model.board }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Cuics"
    , body =
        [ Html.div [ css [ Tw.flex, Tw.flex_col, Tw.justify_center, Tw.items_center, Tw.gap_2, Tw.h_full, Tw.w_full ] ]
            [ viewDice model.diceThrow
            , viewBoard model.board model.turn model.diceThrow
            , Css.Global.global Tw.globalStyles
            ]
            |> Html.toUnstyled
        ]
    }



-- VIEW DICE


type Pips
    = Pips1
    | Pips2
    | Pips3
    | Pips4
    | Pips5
    | Pips6


type DieColor
    = DieWhite
    | DieRed
    | DieYellow
    | DieGreen
    | DieBlue


viewDice : DiceThrow -> Html Msg
viewDice diceThrow =
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2 ] ]
        [ viewDie DieWhite diceThrow.dieWhite1
        , viewDie DieWhite diceThrow.dieWhite2
        , viewDie DieRed diceThrow.dieRed
        , viewDie DieYellow diceThrow.dieYellow
        , viewDie DieGreen diceThrow.dieGreen
        , viewDie DieBlue diceThrow.dieBlue
        ]


viewDie : DieColor -> Pips -> Html Msg
viewDie dieColor pips =
    let
        colors =
            getDieColors dieColor

        pip =
            viewDiePip colors.pip
    in
    Html.div
        [ css [ Tw.w_16, Tw.h_16, Tw.bg_color colors.face, Tw.rounded_2xl ]
        , css [ Tw.border_2, Tw.border_color colors.border ]
        ]
        [ Svg.svg [ Svga.viewBox "-10 -10 20 20" ]
            ([ -- Top left
               mergeIf (List.member pips [ Pips4, Pips5, Pips6 ])
                [ pip -1 -1 ]

             -- Top right
             , mergeIf (List.member pips [ Pips2, Pips3, Pips4, Pips5, Pips6 ])
                [ pip 1 -1 ]

             -- Bottom left
             , mergeIf (List.member pips [ Pips2, Pips3, Pips4, Pips5, Pips6 ])
                [ pip -1 1 ]

             -- Bottom right
             , mergeIf (List.member pips [ Pips4, Pips5, Pips6 ])
                [ pip 1 1 ]

             -- Center
             , mergeIf (List.member pips [ Pips1, Pips3, Pips5 ])
                [ pip 0 0 ]

             -- Left and right
             , mergeIf (List.member pips [ Pips6 ])
                [ pip -1 0
                , pip 1 0
                ]
             ]
                |> List.concat
            )
        ]


viewDiePip : Twc.Color -> Int -> Int -> Svg Msg
viewDiePip twColor xOffset yOffset =
    Svg.circle
        [ Svga.cx (String.fromInt (xOffset * 4))
        , Svga.cy (String.fromInt (yOffset * 4))
        , Svga.r "1.2"
        , css [ Tw.fill_color twColor ]
        ]
        []



-- VIEW BOARD


viewBoard : Board -> Turn -> DiceThrow -> Html Msg
viewBoard board turn diceThrow =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
        [ viewColorRows board turn diceThrow
        , viewFaults ClickedFault (Board.getFaults board)
        , viewScoreboard board
        ]


viewColorRows : Board -> Turn -> DiceThrow -> Html Msg
viewColorRows board turn diceThrow =
    let
        colorRow : Color -> Html Msg
        colorRow color =
            viewColorRow (Board.getRow color board) turn diceThrow color
    in
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
        [ colorRow Red
        , colorRow Yellow
        , colorRow Green
        , colorRow Blue
        ]


viewColorRow : Row -> Turn -> DiceThrow -> Color -> Html Msg
viewColorRow row turn diceThrow color =
    let
        reverse : Bool
        reverse =
            Color.isReverse color

        cell : Num -> Html Msg
        cell num =
            let
                status : CellStatus
                status =
                    getStatus reverse row turn diceThrow color num
            in
            viewColorRowCell (ClickedCell color num) color num status

        cells : List (Html Msg)
        cells =
            (if Color.isReverse color then
                Num.allBackward

             else
                Num.allForward
            )
                |> List.map cell

        colors =
            getColors color Available
    in
    Html.div
        [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.p_1 ]
        , css [ Tw.bg_color colors.fg ]
        ]
        ([ cells
         , [ viewLockCell color (Row.getLock (Color.isReverse color) row) ]
         ]
            |> List.concat
        )


viewColorRowCell : Msg -> Color -> Num -> CellStatus -> Html Msg
viewColorRowCell onClick color num status =
    let
        colors =
            getColors color status

        conditionalStyles =
            case status of
                Available ->
                    [ Events.onClick onClick
                    ]

                Xed ->
                    [ css [ Tw.cursor_default ]
                    , class "xed"
                    ]

                Unavailable ->
                    [ css [ Tw.cursor_not_allowed ] ]
    in
    Html.button
        ([ css [ Tw.w_16, Tw.h_16, Tw.flex, Tw.justify_center, Tw.items_center ]
         , css [ Tw.text_2xl, Tw.text_color colors.fg, Tw.font_bold ]
         , css [ Tw.bg_color colors.bg ]
         , css [ Tw.border_2, Tw.border_color colors.b, Tw.rounded_lg ]
         , css [ Tw.select_none ]
         ]
            ++ conditionalStyles
        )
        [ Html.text (num |> Num.toInt |> String.fromInt) ]


viewLockCell : Color -> Bool -> Html Msg
viewLockCell color xed =
    let
        colors =
            getColors color Available
    in
    Html.div
        [ css [ Tw.w_16, Tw.h_16, Tw.flex, Tw.justify_center, Tw.items_center ]
        , css [ Tw.text_3xl, Tw.text_color colors.fg ]
        , css [ Tw.bg_color colors.bg ]
        , css [ Tw.border_2, Tw.border_color colors.b, Tw.rounded_full ]
        , css [ Tw.select_none ]
        , attributeIf xed (class "xed")
        ]
        [ Html.text "🔓" ]



-- VIEW FAULTS


viewFaults : Msg -> Int -> Html Msg
viewFaults onClick count =
    let
        faultButtons =
            [ 1, 2, 3, 4 ]
                |> List.map (\n -> viewFaultButton onClick (n <= count))
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.justify_end, Tw.items_center ] ]
        ([ [ Html.div [ css [ Tw.mr_3 ] ]
                [ Html.text "Faltas:" ]
           ]
         , faultButtons
         ]
            |> List.concat
        )


viewFaultButton : Msg -> Bool -> Html Msg
viewFaultButton onClick xed =
    let
        conditionalStyles =
            if xed then
                [ css [ Tw.cursor_default ]
                , class "xed"
                ]

            else
                [ Events.onClick onClick ]
    in
    Html.button
        ([ css [ Tw.w_8, Tw.h_8 ]
         , css [ Tw.border_2, Tw.border_color faultColors.fg, Tw.rounded_lg ]
         , attributeIf xed (class "xed")
         ]
            ++ conditionalStyles
        )
        []



-- VIEW SCOREBOARD


viewScoreboard : Board -> Html Msg
viewScoreboard board =
    let
        between : String -> Html Msg
        between string =
            Html.div [ css [ Tw.font_bold ] ]
                [ Html.text string ]
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2, Tw.items_center ] ]
        [ viewScoreboardColorPoints Red (Board.getRow Red board)
        , between "+"
        , viewScoreboardColorPoints Yellow (Board.getRow Yellow board)
        , between "+"
        , viewScoreboardColorPoints Green (Board.getRow Green board)
        , between "+"
        , viewScoreboardColorPoints Blue (Board.getRow Blue board)
        , between "−"
        , viewScoreboardPoints faultColors.fg (Board.getFaults board) (Board.faultPoints board)
        , between "="
        , viewScoreboardSquare Twc.black
            [ Html.div [ css [ Tw.font_bold, Tw.text_2xl, Tw.text_color Twc.black ] ]
                [ Html.text (String.fromInt (Board.points board) ++ " p") ]
            ]
        ]


viewScoreboardColorPoints : Color -> Row -> Html Msg
viewScoreboardColorPoints color row =
    let
        colors =
            getColors color Available
    in
    viewScoreboardPoints
        colors.fg
        (Row.xCount (Color.isReverse color) row)
        (Row.points (Color.isReverse color) row)


viewScoreboardPoints : Twc.Color -> Int -> Int -> Html Msg
viewScoreboardPoints twColor xs points =
    viewScoreboardSquare twColor
        [ Html.div []
            [ Html.text ("{xs} ╳ =" |> String.replace "{xs}" (String.fromInt xs)) ]
        , Html.div [ css [ Tw.font_bold, Tw.text_xl, Tw.text_color twColor ] ]
            [ Html.text ("{points} p" |> String.replace "{points}" (String.fromInt points)) ]
        ]


viewScoreboardSquare : Twc.Color -> List (Html Msg) -> Html Msg
viewScoreboardSquare twColor content =
    Html.div
        [ css [ Tw.flex, Tw.flex_col, Tw.gap_1, Tw.w_24, Tw.items_center, Tw.p_2 ]
        , css [ Tw.border_4, Tw.border_color twColor, Tw.rounded_lg ]
        ]
        content



-- UTILS


getStatus : Bool -> Row -> Turn -> DiceThrow -> Color -> Num -> CellStatus
getStatus reverse row turn diceThrow color num =
    let
        basicAvailability =
            if Row.get num row then
                Xed

            else if cellIsAvailable reverse row num then
                if Num.isLast reverse num then
                    if Row.xCount reverse row >= 5 then
                        Available

                    else
                        Unavailable

                else
                    Available

            else
                Unavailable

        availableNums =
            availableNumsByDiceThrow turn diceThrow color
    in
    case basicAvailability of
        Available ->
            if List.member num availableNums then
                Available

            else
                Unavailable

        _ ->
            basicAvailability


availableNumsByDiceThrow : Turn -> DiceThrow -> Color -> List Num
availableNumsByDiceThrow turn diceThrow color =
    case turn of
        NotTurn ->
            []

        TurnPickingWhite ->
            [ addPips diceThrow.dieWhite1 diceThrow.dieWhite2 ]

        TurnPickingColored ->
            case color of
                Red ->
                    [ addPips diceThrow.dieWhite1 diceThrow.dieRed
                    , addPips diceThrow.dieWhite2 diceThrow.dieRed
                    ]

                Yellow ->
                    [ addPips diceThrow.dieWhite1 diceThrow.dieYellow
                    , addPips diceThrow.dieWhite2 diceThrow.dieYellow
                    ]

                Green ->
                    [ addPips diceThrow.dieWhite1 diceThrow.dieGreen
                    , addPips diceThrow.dieWhite2 diceThrow.dieGreen
                    ]

                Blue ->
                    [ addPips diceThrow.dieWhite1 diceThrow.dieBlue
                    , addPips diceThrow.dieWhite2 diceThrow.dieBlue
                    ]


addPips : Pips -> Pips -> Num
addPips pips1 pips2 =
    case ( pips1, pips2 ) of
        ( Pips1, Pips1 ) ->
            Num2

        ( Pips1, Pips2 ) ->
            Num3

        ( Pips1, Pips3 ) ->
            Num4

        ( Pips1, Pips4 ) ->
            Num5

        ( Pips1, Pips5 ) ->
            Num6

        ( Pips1, Pips6 ) ->
            Num7

        ( Pips2, Pips2 ) ->
            Num4

        ( Pips2, Pips3 ) ->
            Num5

        ( Pips2, Pips4 ) ->
            Num6

        ( Pips2, Pips5 ) ->
            Num7

        ( Pips2, Pips6 ) ->
            Num8

        ( Pips3, Pips3 ) ->
            Num6

        ( Pips3, Pips4 ) ->
            Num7

        ( Pips3, Pips5 ) ->
            Num8

        ( Pips3, Pips6 ) ->
            Num9

        ( Pips4, Pips4 ) ->
            Num8

        ( Pips4, Pips5 ) ->
            Num9

        ( Pips4, Pips6 ) ->
            Num10

        ( Pips5, Pips5 ) ->
            Num10

        ( Pips5, Pips6 ) ->
            Num11

        ( Pips6, Pips6 ) ->
            Num12

        _ ->
            -- Bigger number is the second, so we turn them around and try again.
            addPips pips2 pips1


cellIsAvailable : Bool -> Row -> Num -> Bool
cellIsAvailable reverse row num =
    case ( Row.get num row, Num.next reverse num ) of
        ( True, _ ) ->
            False

        ( False, Just n ) ->
            cellIsAvailable reverse row n

        ( False, Nothing ) ->
            True


mergeIf : Bool -> List a -> List a
mergeIf condition items =
    if condition then
        items

    else
        []


getColors : Color -> CellStatus -> { fg : Twc.Color, bg : Twc.Color, b : Twc.Color }
getColors color status =
    case ( status, color ) of
        ( Available, Red ) ->
            { fg = Twc.red_500, bg = Twc.red_50, b = Twc.red_700 }

        ( Available, Yellow ) ->
            { fg = Twc.yellow_500, bg = Twc.yellow_50, b = Twc.yellow_700 }

        ( Available, Green ) ->
            { fg = Twc.green_500, bg = Twc.green_50, b = Twc.green_700 }

        ( Available, Blue ) ->
            { fg = Twc.blue_500, bg = Twc.blue_50, b = Twc.blue_700 }

        ( Xed, Red ) ->
            { fg = Twc.red_500, bg = Twc.red_50, b = Twc.red_700 }

        ( Xed, Yellow ) ->
            { fg = Twc.yellow_500, bg = Twc.yellow_50, b = Twc.yellow_700 }

        ( Xed, Green ) ->
            { fg = Twc.green_500, bg = Twc.green_50, b = Twc.green_700 }

        ( Xed, Blue ) ->
            { fg = Twc.blue_500, bg = Twc.blue_50, b = Twc.blue_700 }

        ( Unavailable, Red ) ->
            { fg = Twc.red_200, bg = Twc.red_50, b = Twc.red_700 }

        ( Unavailable, Yellow ) ->
            { fg = Twc.yellow_200, bg = Twc.yellow_50, b = Twc.yellow_700 }

        ( Unavailable, Green ) ->
            { fg = Twc.green_200, bg = Twc.green_50, b = Twc.green_700 }

        ( Unavailable, Blue ) ->
            { fg = Twc.blue_200, bg = Twc.blue_50, b = Twc.blue_700 }


faultColors : { fg : Twc.Color, bg : Twc.Color }
faultColors =
    { fg = Twc.gray_400, bg = Twc.gray_50 }


getDieColors : DieColor -> { face : Twc.Color, border : Twc.Color, pip : Twc.Color }
getDieColors dieColor =
    case dieColor of
        DieWhite ->
            { face = Twc.white, border = Twc.gray_300, pip = Twc.black }

        DieRed ->
            { face = Twc.red_500, border = Twc.red_700, pip = Twc.white }

        DieYellow ->
            { face = Twc.yellow_500, border = Twc.yellow_700, pip = Twc.white }

        DieGreen ->
            { face = Twc.green_500, border = Twc.green_700, pip = Twc.white }

        DieBlue ->
            { face = Twc.blue_500, border = Twc.blue_700, pip = Twc.white }
