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
import Random
import Random.Extra as Random
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
    , turn : Turn
    }


type CellStatus
    = Available
    | Xed
    | Picked
    | Unavailable


type Turn
    = NotTurn
    | TurnPicking DiceThrow
    | TurnPickedOnce DiceThrow Pick


type alias DiceThrow =
    { dieWhite1 : Pips
    , dieWhite2 : Pips
    , dieRed : Pips
    , dieYellow : Pips
    , dieGreen : Pips
    , dieBlue : Pips
    }


type alias Pick =
    { color : Color
    , num : Num
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.init
      , turn = NotTurn
      }
    , throwDice
    )



-- UPDATE


type Msg
    = ClickedCell Pick
    | ClickedPickedCell
    | ClickedFault
    | DiceThrown DiceThrow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiceThrown diceThrow ->
            ( { model | turn = TurnPicking diceThrow }
            , Cmd.none
            )

        ClickedCell pick ->
            case model.turn of
                NotTurn ->
                    ( model, Cmd.none )

                TurnPicking diceThrow ->
                    ( { model | turn = TurnPickedOnce diceThrow pick }
                    , Cmd.none
                    )

                TurnPickedOnce _ previousPick ->
                    ( { model
                        | board =
                            model.board
                                |> Board.addX previousPick.color previousPick.num
                                |> Board.addX pick.color pick.num
                        , turn = NotTurn
                      }
                    , Cmd.none
                    )

        ClickedPickedCell ->
            case model.turn of
                TurnPickedOnce diceThrow _ ->
                    ( { model | turn = TurnPicking diceThrow }
                    , Cmd.none
                    )

                NotTurn ->
                    ( model, Cmd.none )

                TurnPicking _ ->
                    ( model, Cmd.none )

        ClickedFault ->
            if canAddFault model.turn then
                ( { model
                    | board = Board.addFault model.board
                    , turn = NotTurn
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Cuics"
    , body =
        [ Html.div [ css [ Tw.flex, Tw.flex_col, Tw.justify_center, Tw.items_center, Tw.gap_2, Tw.h_full, Tw.w_full ] ]
            [ viewDiceIfThrown model.turn
            , viewBoard model.board model.turn
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


viewDiceIfThrown : Turn -> Html Msg
viewDiceIfThrown turn =
    case turn of
        NotTurn ->
            Html.div [ css [ Tw.h_16 ] ] []

        TurnPicking diceThrow ->
            viewDice diceThrow

        TurnPickedOnce diceThrow _ ->
            viewDice diceThrow


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


viewBoard : Board -> Turn -> Html Msg
viewBoard board turn =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
        [ viewColorRows board turn
        , viewFaults (canAddFault turn) (Board.getFaults board)
        , viewScoreboard board
        ]


viewColorRows : Board -> Turn -> Html Msg
viewColorRows board turn =
    let
        colorRow : Color -> Html Msg
        colorRow color =
            viewColorRow (Board.getRow color board) turn color
    in
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
        [ colorRow Red
        , colorRow Yellow
        , colorRow Green
        , colorRow Blue
        ]


viewColorRow : Row -> Turn -> Color -> Html Msg
viewColorRow row turn color =
    let
        growth : Num.Growth
        growth =
            Color.growth color

        cell : Num -> Html Msg
        cell num =
            let
                status : CellStatus
                status =
                    getCellStatus growth row turn color num
            in
            viewColorRowCell (ClickedCell { color = color, num = num }) color num status

        cells : List (Html Msg)
        cells =
            Num.all (Color.growth color)
                |> List.map cell

        colors =
            getColors color Available
    in
    Html.div
        [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.p_1 ]
        , css [ Tw.bg_color colors.fg ]
        ]
        ([ cells
         , [ viewLockCell color (Row.getLock (Color.growth color) row) ]
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
                    [ Events.onClick onClick ]

                Xed ->
                    [ css [ Tw.cursor_default ]
                    , class "xed"
                    ]

                Picked ->
                    [ Events.onClick ClickedPickedCell
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


viewFaults : Bool -> Int -> Html Msg
viewFaults active count =
    let
        faultButtons =
            [ 1, 2, 3, 4 ]
                |> List.map (\n -> viewFaultButton active (n <= count))
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.justify_end, Tw.items_center ] ]
        ([ [ Html.div [ css [ Tw.mr_3 ] ]
                [ Html.text "Faltas:" ]
           ]
         , faultButtons
         ]
            |> List.concat
        )


viewFaultButton : Bool -> Bool -> Html Msg
viewFaultButton active xed =
    let
        conditionalStyles =
            [ mergeIf xed
                [ css [ Tw.cursor_default ]
                , class "xed"
                ]
            , mergeIf active
                [ css [ Tw.border_color faultColors.fg ]
                , Events.onClick ClickedFault
                ]
            , mergeIf (not active)
                [ css [ Tw.border_color faultColors.fgDisabled ] ]
            , mergeIf (not active && not xed)
                [ css [ Tw.cursor_not_allowed ] ]
            ]
                |> List.concat
    in
    Html.button
        ([ css [ Tw.w_8, Tw.h_8 ]
         , css [ Tw.bg_color faultColors.bg ]
         , css [ Tw.border_2, Tw.rounded_lg ]
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
        (Row.xCount (Color.growth color) row)
        (Row.points (Color.growth color) row)


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



-- CELL STATUS


getCellStatus : Num.Growth -> Row -> Turn -> Color -> Num -> CellStatus
getCellStatus growth row turn color num =
    let
        isPicked =
            case turn of
                TurnPickedOnce _ pick ->
                    pick.color == color && pick.num == num

                _ ->
                    False

        basicAvailability =
            if Row.get num row then
                Xed

            else if cellIsAvailable growth row num then
                if Num.isLast growth num then
                    if Row.xCount growth row >= 5 then
                        Available

                    else
                        Unavailable

                else
                    Available

            else
                Unavailable

        availableNums =
            availableNumsByDiceThrow turn color
    in
    if isPicked then
        Picked

    else
        case basicAvailability of
            Available ->
                if List.member num availableNums then
                    Available

                else
                    Unavailable

            _ ->
                basicAvailability


availableNumsByDiceThrow : Turn -> Color -> List Num
availableNumsByDiceThrow turn color =
    case turn of
        NotTurn ->
            []

        TurnPicking diceThrow ->
            getWhitePicks diceThrow ++ getColoredPicks diceThrow color

        TurnPickedOnce diceThrow pick ->
            let
                whitePicks : List Num
                whitePicks =
                    getWhitePicks diceThrow

                coloredPicks : List Num
                coloredPicks =
                    getColoredPicks diceThrow color

                filterPicks : Bool -> List Num -> List Num
                filterPicks toTheLeft =
                    let
                        growth : Num.Growth
                        growth =
                            case ( toTheLeft, Color.growth color ) of
                                ( True, growth_ ) ->
                                    growth_

                                ( False, Num.Grows ) ->
                                    Num.Shrinks

                                ( False, Num.Shrinks ) ->
                                    Num.Grows

                        rowWithXedPick : Row
                        rowWithXedPick =
                            Row.init |> Row.set pick.num True
                    in
                    List.filter (cellIsAvailable growth rowWithXedPick)
            in
            case getFirstPickType diceThrow pick of
                FirstPickedWhite ->
                    if color == pick.color then
                        -- Remove all cells to the left.
                        filterPicks True coloredPicks

                    else
                        coloredPicks

                FirstPickedColored ->
                    if color == pick.color then
                        -- Remove all cells to the right. This is because
                        -- the rules say you have to first pick white, so the
                        -- previous move was made in the wrong order and we have
                        -- to accomodate for that.
                        filterPicks False whitePicks

                    else
                        whitePicks

                FirstPickedEither ->
                    whitePicks ++ coloredPicks


type FirstPickType
    = FirstPickedWhite
    | FirstPickedColored
    | FirstPickedEither


getFirstPickType : DiceThrow -> Pick -> FirstPickType
getFirstPickType diceThrow pick =
    let
        pickedWhite =
            List.member pick.num (getWhitePicks diceThrow)

        pickedColored =
            getColoredPicks diceThrow pick.color
                |> List.member pick.num
    in
    if pickedWhite && not pickedColored then
        FirstPickedWhite

    else if pickedColored && not pickedWhite then
        FirstPickedColored

    else
        FirstPickedEither


getWhitePicks : DiceThrow -> List Num
getWhitePicks diceThrow =
    [ addPips diceThrow.dieWhite1 diceThrow.dieWhite2 ]


getColoredPicks : DiceThrow -> Color -> List Num
getColoredPicks diceThrow color =
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


cellIsAvailable : Num.Growth -> Row -> Num -> Bool
cellIsAvailable growth row num =
    case ( Row.get num row, Num.next growth num ) of
        ( True, _ ) ->
            False

        ( False, Just n ) ->
            cellIsAvailable growth row n

        ( False, Nothing ) ->
            True



-- UTILS


throwDice : Cmd Msg
throwDice =
    Random.generate DiceThrown diceThrowGenerator


diceThrowGenerator : Random.Generator DiceThrow
diceThrowGenerator =
    Random.constant DiceThrow
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator


pipsGenerator : Random.Generator Pips
pipsGenerator =
    Random.int 1 6
        |> Random.map
            (\n ->
                case n of
                    1 ->
                        Pips1

                    2 ->
                        Pips2

                    3 ->
                        Pips3

                    4 ->
                        Pips4

                    5 ->
                        Pips5

                    _ ->
                        Pips6
            )


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


canAddFault : Turn -> Bool
canAddFault turn =
    case turn of
        TurnPicking _ ->
            True

        NotTurn ->
            False

        TurnPickedOnce _ _ ->
            False


mergeIf : Bool -> List a -> List a
mergeIf condition items =
    if condition then
        items

    else
        []



-- COLORS


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

        ( Picked, Red ) ->
            { fg = Twc.red_500, bg = Twc.black, b = Twc.red_700 }

        ( Picked, Yellow ) ->
            { fg = Twc.yellow_500, bg = Twc.black, b = Twc.yellow_700 }

        ( Picked, Green ) ->
            { fg = Twc.green_500, bg = Twc.black, b = Twc.green_700 }

        ( Picked, Blue ) ->
            { fg = Twc.blue_500, bg = Twc.black, b = Twc.blue_700 }

        ( Unavailable, Red ) ->
            { fg = Twc.red_200, bg = Twc.red_50, b = Twc.red_700 }

        ( Unavailable, Yellow ) ->
            { fg = Twc.yellow_200, bg = Twc.yellow_50, b = Twc.yellow_700 }

        ( Unavailable, Green ) ->
            { fg = Twc.green_200, bg = Twc.green_50, b = Twc.green_700 }

        ( Unavailable, Blue ) ->
            { fg = Twc.blue_200, bg = Twc.blue_50, b = Twc.blue_700 }


faultColors =
    { fg = Twc.gray_400, fgDisabled = Twc.gray_200, bg = Twc.gray_50 }


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
