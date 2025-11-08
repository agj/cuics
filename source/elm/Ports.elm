port module Ports exposing (saveSettings)

import Json.Encode as Encode
import Language exposing (Language)



-- OUTGOING


saveSettings : { language : Maybe Language } -> Cmd msg
saveSettings settings =
    sendOut "saveSettings"
        (Encode.object
            [ ( "language", Language.encodeMaybe settings.language ) ]
        )



-- INTERNAL


sendOut : String -> Encode.Value -> Cmd msg
sendOut msg value =
    sendToJs
        (Encode.object
            [ ( "msg", Encode.string msg )
            , ( "value", value )
            ]
        )



-- PORTS


port sendToJs : Encode.Value -> Cmd msg
