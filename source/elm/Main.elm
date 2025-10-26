module Main exposing (main)

import Browser
import Html
import Html.Attributes exposing (class)


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
        [ Html.button [ class "button" ] [ Html.text "Hola" ] ]
    }
