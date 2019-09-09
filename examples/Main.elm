module Main exposing (main)

import Color
import Html exposing (text)
import Legend.Continuous as Continuous
import Legend.Legend as Legend
import Legend.Segments as Segments


main =
    let
        bins =
            Legend.toBins [ 0, 1, 2, 3 ] [ Color.red, Color.green, Color.blue, Color.orange ]
    in
    Html.div [] [ Continuous.view bins String.fromInt, Segments.view bins String.fromInt ]
