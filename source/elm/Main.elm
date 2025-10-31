module Main exposing (main)

import Browser
import Css.Global
import Dict.Any exposing (AnyDict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Events as Events
import List exposing (range)
import Tailwind.Theme as Twc
import Tailwind.Utilities as Tw


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
    { redRow : RowXs
    , yellowRow : RowXs
    , greenRow : RowXs
    , blueRow : RowXs
    }


type Color
    = Red
    | Yellow
    | Green
    | Blue


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


type CellStatus
    = Available
    | Xed
    | Unavailable


type alias RowXs =
    AnyDict Int Num ()



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { redRow = Dict.Any.empty numToInt
      , yellowRow = Dict.Any.empty numToInt
      , greenRow = Dict.Any.empty numToInt
      , blueRow = Dict.Any.empty numToInt
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedCell Color Num


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell color num ->
            let
                newModel =
                    case color of
                        Red ->
                            { model | redRow = xNum model.redRow num }

                        Yellow ->
                            { model | yellowRow = xNum model.yellowRow num }

                        Green ->
                            { model | greenRow = xNum model.greenRow num }

                        Blue ->
                            { model | blueRow = xNum model.blueRow num }
            in
            ( newModel, Cmd.none )



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


viewBoard : Model -> Html Msg
viewBoard model =
    viewColorRows model


viewColorRows : Model -> Html Msg
viewColorRows model =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1, Tw.p_2 ] ]
        [ viewColorRow (ClickedCell Red) model.redRow Red
        , viewColorRow (ClickedCell Yellow) model.yellowRow Yellow
        , viewColorRow (ClickedCell Green) model.greenRow Green
        , viewColorRow (ClickedCell Blue) model.blueRow Blue
        ]


viewColorRow : (Num -> Msg) -> RowXs -> Color -> Html Msg
viewColorRow onClick rowXs color =
    let
        cell : Num -> Html Msg
        cell num =
            let
                status : CellStatus
                status =
                    getStatus rowXs num
            in
            viewColorRowCell (onClick num) color num status

        colors =
            getColors color Available
    in
    Html.div
        [ css [ Tw.flex, Tw.flex_row, Tw.gap_1 ]
        , css [ Tw.bg_color colors.b ]
        , css [ Tw.border_2, Tw.border_color colors.b ]
        ]
        [ cell Num2
        , cell Num3
        , cell Num4
        , cell Num5
        , cell Num6
        , cell Num7
        , cell Num8
        , cell Num9
        , cell Num10
        , cell Num11
        , cell Num12
        , Html.div
            [ css [ Tw.w_16, Tw.h_16, Tw.flex, Tw.justify_center, Tw.items_center ]
            , css [ Tw.text_3xl, Tw.text_color colors.fg ]
            , css [ Tw.bg_color colors.bg ]
            , css [ Tw.border_2, Tw.border_color colors.b, Tw.rounded_full ]
            , css [ Tw.select_none ]
            ]
            [ Html.text "🔓" ]
        ]


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
        [ Html.text (num |> numToInt |> String.fromInt) ]



-- UTILS


getStatus : RowXs -> Num -> CellStatus
getStatus rowXs num =
    if cellIsXed rowXs num then
        Xed

    else if cellIsAvailable rowXs num then
        Available

    else
        Unavailable


cellIsAvailable : RowXs -> Num -> Bool
cellIsAvailable rowXs num =
    case ( cellIsXed rowXs num, nextNum num ) of
        ( True, _ ) ->
            False

        ( False, Just n ) ->
            cellIsAvailable rowXs n

        ( False, Nothing ) ->
            True


cellIsXed : RowXs -> Num -> Bool
cellIsXed rowXs num =
    case Dict.Any.get num rowXs of
        Just () ->
            True

        Nothing ->
            False


xNum : RowXs -> Num -> RowXs
xNum rowStatus num =
    rowStatus |> Dict.Any.insert num ()


getColors : Color -> CellStatus -> { fg : Twc.Color, bg : Twc.Color, b : Twc.Color }
getColors color status =
    case ( status, color ) of
        ( Available, Red ) ->
            { fg = Twc.red_500, bg = Twc.red_50, b = Twc.red_800 }

        ( Available, Yellow ) ->
            { fg = Twc.yellow_500, bg = Twc.yellow_50, b = Twc.yellow_800 }

        ( Available, Green ) ->
            { fg = Twc.green_500, bg = Twc.green_50, b = Twc.green_800 }

        ( Available, Blue ) ->
            { fg = Twc.blue_500, bg = Twc.blue_50, b = Twc.blue_800 }

        ( Xed, Red ) ->
            { fg = Twc.red_500, bg = Twc.red_50, b = Twc.red_800 }

        ( Xed, Yellow ) ->
            { fg = Twc.yellow_500, bg = Twc.yellow_50, b = Twc.yellow_800 }

        ( Xed, Green ) ->
            { fg = Twc.green_500, bg = Twc.green_50, b = Twc.green_800 }

        ( Xed, Blue ) ->
            { fg = Twc.blue_500, bg = Twc.blue_50, b = Twc.blue_800 }

        ( Unavailable, Red ) ->
            { fg = Twc.red_300, bg = Twc.red_50, b = Twc.red_300 }

        ( Unavailable, Yellow ) ->
            { fg = Twc.yellow_300, bg = Twc.yellow_50, b = Twc.yellow_300 }

        ( Unavailable, Green ) ->
            { fg = Twc.green_300, bg = Twc.green_50, b = Twc.green_300 }

        ( Unavailable, Blue ) ->
            { fg = Twc.blue_300, bg = Twc.blue_50, b = Twc.blue_300 }


numToInt : Num -> Int
numToInt num =
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


nextNum : Num -> Maybe Num
nextNum num =
    case num of
        Num2 ->
            Just Num3

        Num3 ->
            Just Num4

        Num4 ->
            Just Num5

        Num5 ->
            Just Num6

        Num6 ->
            Just Num7

        Num7 ->
            Just Num8

        Num8 ->
            Just Num9

        Num9 ->
            Just Num10

        Num10 ->
            Just Num11

        Num11 ->
            Just Num12

        Num12 ->
            Nothing
