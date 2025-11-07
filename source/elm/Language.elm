module Language exposing (Language(..), decoder, decoderFromList)

import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra


type Language
    = Spanish
    | English
    | Japanese
    | ChineseTraditional


decoderFromList : Decoder Language
decoderFromList =
    Decode.list (Decode.maybe decoder)
        |> Decode.andThen
            (\list ->
                case list |> Maybe.Extra.values |> List.head of
                    Just language ->
                        Decode.succeed language

                    Nothing ->
                        Decode.fail "Language not supported"
            )


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
