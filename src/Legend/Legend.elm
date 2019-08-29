module Legend.Legend exposing (..)

import Color exposing (Color, rgb255)
import Html


type alias Bin a =
    { value : a
    , colour : Color
    }


view : List (Bin a) -> (a -> String) -> Html.Html b
view bins show =
    Html.div [] (List.map (\bin -> Html.text <| show bin.value) bins)
