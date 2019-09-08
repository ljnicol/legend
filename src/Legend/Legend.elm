module Legend.Legend exposing (Bin, Config, toBin, toBins)

import Color


type alias Bin a =
    { value : a
    , color : Color.Color
    }


toBin : a -> Color.Color -> Bin a
toBin v c =
    { value = v, color = c }


toBins : List a -> List Color.Color -> List (Bin a)
toBins vs cs =
    List.map2 toBin vs cs


type alias Config =
    { numberOfStops : Int
    , numberOfTicks : Int
    , segmentWidth : Int
    , padding : Int
    , svgHeight : Float
    , unitsYTranslate : Float
    , textTopPadding : Int
    , segmentHeight : Int
    , colorYTranslate : Float
    , tickWidth : Int
    }
