module Texts exposing (Texts, for)

import Language exposing (Language(..))


type alias Texts =
    { done : String
    , faults : String
    , p : String
    }


for : Language -> Texts
for language =
    case language of
        Spanish ->
            spanish

        English ->
            english

        Japanese ->
            japanese

        ChineseTraditional ->
            chineseTraditional


spanish : Texts
spanish =
    { done = "Terminar turno"
    , faults = "Faltas"
    , p = " p"
    }


english : Texts
english =
    { done = "End turn"
    , faults = "Faults"
    , p = " p"
    }


japanese : Texts
japanese =
    { done = "決定"
    , faults = "欠点"
    , p = "点"
    }


chineseTraditional : Texts
chineseTraditional =
    { done = "決定"
    , faults = "錯誤"
    , p = "分"
    }
