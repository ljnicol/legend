module Legend.Legend exposing (..)

import Color

type alias Bin a =
    { value : a
    , color : Color.Color
    }

toBin : a -> Color.Color -> Bin a
toBin v c =
    {value = v, color = c}

toBins : List a -> List Color.Color -> List (Bin a)
toBins vs cs =
    List.map2 toBin vs cs
