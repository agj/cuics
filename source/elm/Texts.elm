module Texts exposing (Texts, for)

import Language exposing (Language(..))


type alias Texts =
    { done : String
    , faults : String
    , p : String
    , default : String
    , language : String
    , close : String
    , gameOver : String
    , yourFinalScore : Int -> { yourFinalScore : String, score : String, period : String }
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
    , close = "Cerrar"
    , gameOver = "Se terminó el juego"
    , yourFinalScore =
        \p ->
            { yourFinalScore = "Tu puntaje final: "
            , score =
                "{p} p"
                    |> String.replace "{p}" (String.fromInt p)
            , period = "."
            }
    }


english : Texts
english =
    { done = "End turn"
    , faults = "Faults"
    , p = " p"
    , default = "Default"
    , language = "Language"
    , close = "Close"
    , gameOver = "Game over!"
    , yourFinalScore =
        \p ->
            { yourFinalScore = "Your final score: "
            , score =
                "{p} p"
                    |> String.replace "{p}" (String.fromInt p)
            , period = "."
            }
    }


japanese : Texts
japanese =
    { done = "決定"
    , faults = "欠点"
    , p = "点"
    , default = "初期設定"
    , language = "言語"
    , close = "閉じる"
    , gameOver = "ゲームオーバー"
    , yourFinalScore =
        \p ->
            { yourFinalScore = "最終点数："
            , score =
                "{p}点"
                    |> String.replace "{p}" (String.fromInt p)
            , period = "。"
            }
    }


chineseTraditional : Texts
chineseTraditional =
    { done = "決定"
    , faults = "錯誤"
    , p = "分"
    , default = "預設值"
    , language = "語言"
    , close = "關閉"
    , gameOver = "遊戲結束了！"
    , yourFinalScore =
        \p ->
            { yourFinalScore = "總分數："
            , score =
                "{p}分"
                    |> String.replace "{p}" (String.fromInt p)
            , period = "。"
            }
    }


chineseSimplified : Texts
chineseSimplified =
    { done = "决定"
    , faults = "错误"
    , p = "分"
    , default = "预设值"
    , language = "语言"
    , close = "关闭"
    , gameOver = "游戏结束了！"
    , yourFinalScore =
        \p ->
            { yourFinalScore = "总分数："
            , score =
                "{p}分"
                    |> String.replace "{p}" (String.fromInt p)
            , period = "。"
            }
    }
