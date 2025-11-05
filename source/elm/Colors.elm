module Colors exposing (Colors, get, init, set, update)

import Color exposing (Color(..))


type Colors a
    = Colors
        { red : a
        , yellow : a
        , green : a
        , blue : a
        }


init : a -> Colors a
init value =
    Colors
        { red = value
        , yellow = value
        , green = value
        , blue = value
        }


get : Color -> Colors a -> a
get color (Colors colors) =
    case color of
        Red ->
            colors.red

        Yellow ->
            colors.yellow

        Green ->
            colors.green

        Blue ->
            colors.blue


set : Color -> a -> Colors a -> Colors a
set color value (Colors colors) =
    case color of
        Red ->
            Colors { colors | red = value }

        Yellow ->
            Colors { colors | yellow = value }

        Green ->
            Colors { colors | green = value }

        Blue ->
            Colors { colors | blue = value }


update : Color -> (a -> a) -> Colors a -> Colors a
update color updater colors =
    set color (updater (get color colors)) colors
