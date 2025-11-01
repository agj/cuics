module Main exposing (main)

import Array
import Board exposing (Board)
import Browser
import Color exposing (Color(..))
import Css.Global
import Dict.Any exposing (AnyDict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Events as Events
import List exposing (range)
import Num exposing (Num(..))
import Row exposing (Row)
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
    { board : Board }


type CellStatus
    = Available
    | Xed
    | Unavailable


type alias RowXs =
    Row Bool



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.init }
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
        [ Html.div [ css [ Tw.flex, Tw.flex_col, Tw.justify_center, Tw.items_center, Tw.h_full, Tw.w_full ] ]
            [ viewBoard model
            , Css.Global.global Tw.globalStyles
            ]
            |> Html.toUnstyled
        ]
    }



-- VIEW BOARD


viewBoard : Model -> Html Msg
viewBoard model =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
        [ viewColorRows model
        , viewFaults ClickedFault (Board.getFaults model.board)
        , viewScoreboard model
        ]


viewColorRows : Model -> Html Msg
viewColorRows model =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
        [ viewColorRow (ClickedCell Red) False (Board.getRow Red model.board) Red
        , viewColorRow (ClickedCell Yellow) False (Board.getRow Yellow model.board) Yellow
        , viewColorRow (ClickedCell Green) True (Board.getRow Green model.board) Green
        , viewColorRow (ClickedCell Blue) True (Board.getRow Blue model.board) Blue
        ]


viewColorRow : (Num -> Msg) -> Bool -> RowXs -> Color -> Html Msg
viewColorRow onClick reverse rowXs color =
    let
        cell : Num -> Html Msg
        cell num =
            let
                status : CellStatus
                status =
                    getStatus reverse rowXs num
            in
            viewColorRowCell (onClick num) color num status

        cells : List (Html Msg)
        cells =
            (if reverse then
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
         , [ viewLockCell color (getStatus reverse rowXs Num12 == Xed) ]
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


viewScoreboard : Model -> Html Msg
viewScoreboard model =
    let
        between : String -> Html Msg
        between string =
            Html.div [ css [ Tw.font_bold ] ]
                [ Html.text string ]

        faultPoints : Int
        faultPoints =
            Board.getFaults model.board * 5

        totalPoints : Int
        totalPoints =
            ([ Board.getRow Red model.board
             , Board.getRow Yellow model.board
             , Board.getRow Green model.board
             , Board.getRow Blue model.board
             ]
                |> List.map (\rowXs -> getPoints (getXs rowXs))
                |> List.foldl (+) 0
            )
                - faultPoints
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2, Tw.items_center ] ]
        [ viewScoreboardColorPoints Red (Board.getRow Red model.board)
        , between "+"
        , viewScoreboardColorPoints Yellow (Board.getRow Yellow model.board)
        , between "+"
        , viewScoreboardColorPoints Green (Board.getRow Green model.board)
        , between "+"
        , viewScoreboardColorPoints Blue (Board.getRow Blue model.board)
        , between "−"
        , viewScoreboardPoints faultColors.fg (Board.getFaults model.board) faultPoints
        , between "="
        , viewScoreboardSquare Twc.black
            [ Html.div [ css [ Tw.font_bold, Tw.text_2xl, Tw.text_color Twc.black ] ]
                [ Html.text (String.fromInt totalPoints ++ " p") ]
            ]
        ]


viewScoreboardColorPoints : Color -> RowXs -> Html Msg
viewScoreboardColorPoints color rowXs =
    let
        colors =
            getColors color Available

        xs : Int
        xs =
            getXs rowXs
    in
    viewScoreboardPoints colors.fg xs (getPoints xs)


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


getStatus : Bool -> RowXs -> Num -> CellStatus
getStatus reverse rowXs num =
    if Row.get num rowXs then
        Xed

    else if cellIsAvailable reverse rowXs num then
        Available

    else
        Unavailable


cellIsAvailable : Bool -> RowXs -> Num -> Bool
cellIsAvailable reverse rowXs num =
    case ( Row.get num rowXs, Num.next reverse num ) of
        ( True, _ ) ->
            False

        ( False, Just n ) ->
            cellIsAvailable reverse rowXs n

        ( False, Nothing ) ->
            True


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


getXs : RowXs -> Int
getXs rowXs =
    Row.count identity rowXs
        + (if Row.get Num12 rowXs then
            1

           else
            0
          )


getPoints : Int -> Int
getPoints xs =
    pointsTable
        |> Array.get (xs - 1)
        |> Maybe.withDefault 0


pointsTable : Array.Array number
pointsTable =
    [ 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78 ]
        |> Array.fromList
