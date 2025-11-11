module Palette exposing (colorBlue, colorGray, colorGreen, colorPurple, colorRed, colorYellow)

import Tailwind.Theme as Twt


colorRed =
    { medium = Twt.red_500
    , dark = Twt.red_700
    , light = Twt.red_200
    , pale = Twt.red_50
    }


colorYellow =
    { medium = Twt.yellow_500
    , dark = Twt.yellow_700
    , light = Twt.yellow_200
    , pale = Twt.yellow_50
    }


colorGreen =
    { medium = Twt.green_500
    , dark = Twt.green_700
    , light = Twt.green_200
    , pale = Twt.green_50
    }


colorBlue =
    { medium = Twt.blue_500
    , dark = Twt.blue_700
    , light = Twt.blue_200
    , pale = Twt.blue_50
    }


colorGray =
    { medium = Twt.gray_400
    , dark = Twt.gray_700
    , light = Twt.gray_200
    , pale = Twt.gray_50
    }


colorPurple =
    { medium = Twt.purple_500
    , dark = Twt.purple_700
    , mediumLight = Twt.purple_300
    , light = Twt.purple_100
    , pale = Twt.purple_50
    }
