module Language exposing (Language(..), Selection, all, code, decoder, decoderFromList, default, defaultSelection, encode, encodeMaybe, name, selected, selectionDecoder, selectionToLanguage, setSelection)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra


type Language
    = Spanish
    | English
    | Japanese
    | ChineseTraditional


type Selection
    = Selection
        { default : Language
        , selected : Maybe Language
        }


all : List Language
all =
    [ Spanish
    , English
    , Japanese
    , ChineseTraditional
    ]


default : Language
default =
    English


defaultSelection : Selection
defaultSelection =
    Selection { default = default, selected = Nothing }


name : Language -> String
name language =
    case language of
        Spanish ->
            "Español"

        English ->
            "English"

        Japanese ->
            "日本語"

        ChineseTraditional ->
            "繁體中文"


code : Language -> String
code language =
    case language of
        Spanish ->
            "es"

        English ->
            "en"

        Japanese ->
            "ja"

        ChineseTraditional ->
            "zh"


selectionToLanguage : Selection -> Language
selectionToLanguage (Selection selection) =
    selection.selected
        |> Maybe.withDefault selection.default


setSelection : Maybe Language -> Selection -> Selection
setSelection selected_ (Selection selection) =
    Selection { selection | selected = selected_ }


selected : Selection -> Maybe Language
selected (Selection selection) =
    selection.selected



-- JSON


decoder : Decoder Language
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                if String.startsWith "en" string then
                    Decode.succeed English

                else if String.startsWith "es" string then
                    Decode.succeed Spanish

                else if String.startsWith "ja" string then
                    Decode.succeed Japanese

                else if String.startsWith "zh" string then
                    Decode.succeed ChineseTraditional

                else
                    Decode.fail "Language not supported"
            )


decoderFromList : Decoder Language
decoderFromList =
    Decode.list (Decode.maybe decoder)
        |> Decode.andThen
            (\list ->
                case List.head (Maybe.Extra.values list) of
                    Just language ->
                        Decode.succeed language

                    Nothing ->
                        Decode.fail "Language not supported"
            )


selectionDecoder : Decoder Selection
selectionDecoder =
    Decode.map2 (\default_ selected_ -> Selection { default = default_, selected = selected_ })
        (Decode.field "default" decoderFromList)
        (Decode.field "selected" (Decode.maybe decoder))


encode : Language -> Decode.Value
encode language =
    Encode.string (code language)


encodeMaybe : Maybe Language -> Decode.Value
encodeMaybe language =
    case language of
        Just lang ->
            encode lang

        Nothing ->
            Encode.null
