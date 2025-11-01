module Num exposing (Num(..), allBackward, allForward)


type Num
    = Num2
    | Num3
    | Num4
    | Num5
    | Num6
    | Num7
    | Num8
    | Num9
    | Num10
    | Num11
    | Num12


allForward : List Num
allForward =
    [ Num2
    , Num3
    , Num4
    , Num5
    , Num6
    , Num7
    , Num8
    , Num9
    , Num10
    , Num11
    , Num12
    ]


allBackward : List Num
allBackward =
    [ Num12
    , Num11
    , Num10
    , Num9
    , Num8
    , Num7
    , Num6
    , Num5
    , Num4
    , Num3
    , Num2
    ]
