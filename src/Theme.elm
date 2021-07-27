module Theme exposing (color)

import Element exposing (Color, rgb255)


type alias ThemeColors =
    { grey : Color
    , blueHex : String
    , lightBlueHex : String
    }


color : ThemeColors
color =
    { grey = rgb255 211 211 211
    , blueHex = "#020d4c"
    , lightBlueHex = "#405cd4"
    }
