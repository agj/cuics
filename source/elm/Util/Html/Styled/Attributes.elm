module Util.Html.Styled.Attributes exposing (attributeIf)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes


attributeIf : Bool -> Attribute msg -> Attribute msg
attributeIf condition attr =
    if condition then
        attr

    else
        empty


empty : Attribute msg
empty =
    Attributes.classList []
