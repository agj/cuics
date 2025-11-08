module Texts exposing (Texts, for)

import Language exposing (Language(..))


type alias Texts =
    { done : String
    , faults : String
    , p : String
    , default : String
    , language : String
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

        ChineseSimplified ->
            chineseSimplified


spanish : Texts
spanish =
    { done = "Terminar turno"
    , faults = "Faltas"
    , p = " p"
    , default = "Por defecto"
    , language = "Idioma"
    }


english : Texts
english =
    { done = "End turn"
    , faults = "Faults"
    , p = " p"
    , default = "Default"
    , language = "Language"
    }


japanese : Texts
japanese =
    { done = "決定"
    , faults = "欠点"
    , p = "点"
    , default = "初期設定"
    , language = "言語"
    }


chineseTraditional : Texts
chineseTraditional =
    { done = "決定"
    , faults = "錯誤"
    , p = "分"
    , default = "預設值"
    , language = "語言"
    }


chineseSimplified : Texts
chineseSimplified =
    { done = "决定"
    , faults = "错误"
    , p = "分"
    , default = "预设值"
    , language = "语言"
    }
