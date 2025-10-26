module Main exposing (main)

import Browser
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
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
    {}



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Cuics"
    , body =
        [ Html.div []
            [ viewColorRows
            , Css.Global.global Tw.globalStyles
            ]
            |> Html.toUnstyled
        ]
    }


viewColorRows : Html Msg
viewColorRows =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
        [ viewColorRow Twc.red_800 Twc.red_200
        , viewColorRow Twc.yellow_800 Twc.yellow_200
        , viewColorRow Twc.green_800 Twc.green_200
        , viewColorRow Twc.blue_800 Twc.blue_200
        ]


viewColorRow : Twc.Color -> Twc.Color -> Html Msg
viewColorRow fgColor bgColor =
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_1 ] ]
        ((List.range 2 12
            |> List.map (viewColorRowCell fgColor bgColor)
         )
            ++ [ Html.div
                    [ css
                        [ Tw.rounded_full
                        , Tw.bg_color bgColor
                        , Tw.text_color fgColor
                        , Tw.border_2
                        , Tw.border_color fgColor
                        , Tw.w_16
                        , Tw.h_16
                        , Tw.flex
                        , Tw.justify_center
                        , Tw.items_center
                        , Tw.text_3xl
                        ]
                    ]
                    [ Html.text "🔓" ]
               ]
        )


viewColorRowCell : Twc.Color -> Twc.Color -> Int -> Html Msg
viewColorRowCell fgColor bgColor number =
    Html.div
        [ css
            [ Tw.w_16
            , Tw.h_16
            , Tw.flex
            , Tw.justify_center
            , Tw.items_center
            , Tw.font_bold
            , Tw.text_color fgColor
            , Tw.bg_color bgColor
            , Tw.border_2
            , Tw.border_color fgColor
            , Tw.rounded_lg
            ]
        ]
        [ Html.text (String.fromInt number) ]
