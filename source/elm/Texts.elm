module Texts exposing (Language(..), Texts, for)


type alias Texts =
    { done : String
    , faults : String
    , p : String
    }


type Language
    = Spanish
    | English
    | Japanese
    | ChineseTraditional


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
    { done = "Listo"
    , faults = "Faltas"
    , p = " p"
    }


english : Texts
english =
    { done = "Done"
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
    { done = "結束"
    , faults = "錯誤"
    , p = "分"
    }
