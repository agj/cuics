module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import List exposing (range)


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
        [ Html.div [ class "helvetica" ]
            [ viewColorRow ]
        ]
    }


viewColorRow : Html Msg
viewColorRow =
    Html.div [ class "flex" ]
        (List.range 2 12
            |> List.map viewColorRowCell
        )


viewColorRowCell : Int -> Html Msg
viewColorRowCell number =
    Html.div
        [ class "w3 h3 flex justify-center items-center"
        , class "b dark-red"
        , class "bg-washed-red"
        , class "ba bw1 b--dark-red br3"
        ]
        [ Html.text (String.fromInt number) ]
