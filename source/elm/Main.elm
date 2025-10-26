module Main exposing (main)

import Browser
import Css.Global
import Dict.Any exposing (AnyDict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
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
        [ Html.div []
            [ viewColorRows model
            , Css.Global.global Tw.globalStyles
            ]
            |> Html.toUnstyled
        ]
    }


viewColorRows : Model -> Html Msg
viewColorRows model =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
        [ viewColorRow (ClickedCell Red) Twc.red_800 Twc.red_200 model.redRow
        , viewColorRow (ClickedCell Yellow) Twc.yellow_800 Twc.yellow_200 model.yellowRow
        , viewColorRow (ClickedCell Green) Twc.green_800 Twc.green_200 model.greenRow
        , viewColorRow (ClickedCell Blue) Twc.blue_800 Twc.blue_200 model.blueRow
        ]


viewColorRow : (Num -> Msg) -> Twc.Color -> Twc.Color -> RowXs -> Html Msg
viewColorRow onClick fgColor bgColor rowXs =
    let
        cell : Num -> Html Msg
        cell num =
            let
                status : CellStatus
                status =
                    getStatus rowXs num
            in
            viewColorRowCell (onClick num) fgColor bgColor num status
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_1 ] ]
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
            , css [ Tw.text_3xl, Tw.text_color fgColor ]
            , css [ Tw.bg_color bgColor ]
            , css [ Tw.border_2, Tw.border_color fgColor, Tw.rounded_full ]
            , css [ Tw.select_none ]
            ]
            [ Html.text "🔓" ]
        ]


viewColorRowCell : Msg -> Twc.Color -> Twc.Color -> Num -> CellStatus -> Html Msg
viewColorRowCell onClick fgColor bgColor num status =
    let
        conditionalStyles =
            case status of
                Available ->
                    [ Events.onClick onClick ]

                Xed ->
                    [ css [ Tw.bg_color Twc.gray_400 ] ]

                Unavailable ->
                    [ css [ Tw.bg_color Twc.gray_800 ] ]
    in
    Html.button
        ([ css [ Tw.w_16, Tw.h_16, Tw.flex, Tw.justify_center, Tw.items_center ]
         , css [ Tw.font_bold, Tw.text_color fgColor ]
         , css [ Tw.bg_color bgColor ]
         , css [ Tw.border_2, Tw.border_color fgColor, Tw.rounded_lg ]
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
