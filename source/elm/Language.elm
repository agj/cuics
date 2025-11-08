module Language exposing (Language(..), Selection, all, code, decoder, decoderFromList, default, defaultSelection, encode, encodeMaybe, name, selected, selectionDecoder, selectionToLanguage, setSelection)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LanguageTag exposing (emptySubtags)
import LanguageTag.Language
import LanguageTag.Parser
import LanguageTag.Region
import LanguageTag.Script
import Maybe.Extra


type Language
    = Spanish
    | English
    | Japanese
    | ChineseTraditional
    | ChineseSimplified


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
    , ChineseSimplified
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

        ChineseSimplified ->
            "简体中文"


code : Language -> String
code language =
    case language of
        Spanish ->
            LanguageTag.Language.es
                |> LanguageTag.build emptySubtags
                |> LanguageTag.toString

        English ->
            LanguageTag.Language.en
                |> LanguageTag.build emptySubtags
                |> LanguageTag.toString

        Japanese ->
            LanguageTag.Language.ja
                |> LanguageTag.build emptySubtags
                |> LanguageTag.toString

        ChineseTraditional ->
            LanguageTag.Language.zh
                |> LanguageTag.build { emptySubtags | script = Just LanguageTag.Script.hant }
                |> LanguageTag.toString

        ChineseSimplified ->
            LanguageTag.Language.zh
                |> LanguageTag.build { emptySubtags | script = Just LanguageTag.Script.hans }
                |> LanguageTag.toString


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
                let
                    parsed =
                        LanguageTag.Parser.parseBcp47 string

                    fail =
                        Decode.fail "Language not supported"
                in
                case parsed of
                    Just ( language, options ) ->
                        if language == LanguageTag.Language.en then
                            Decode.succeed English

                        else if language == LanguageTag.Language.es then
                            Decode.succeed Spanish

                        else if language == LanguageTag.Language.ja then
                            Decode.succeed Japanese

                        else if language == LanguageTag.Language.zh then
                            if
                                (options.script == Just LanguageTag.Script.hant)
                                    || (options.region == Just LanguageTag.Region.tw)
                                    || (options.region == Just LanguageTag.Region.hk)
                                    || (options.region == Just LanguageTag.Region.mo)
                            then
                                Decode.succeed ChineseTraditional

                            else
                                Decode.succeed ChineseSimplified

                        else
                            fail

                    Nothing ->
                        fail
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
