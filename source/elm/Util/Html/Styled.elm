module Util.Html.Styled exposing (..)

import Html.Styled as Html exposing (Html)


viewIf : Bool -> Html msg -> Html msg
viewIf condition html =
    if condition then
        html

    else
        Html.text ""


viewIfLazy : Bool -> (() -> Html msg) -> Html msg
viewIfLazy condition htmlThunk =
    if condition then
        htmlThunk ()

    else
        Html.text ""
