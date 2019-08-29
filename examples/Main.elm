import Html exposing (text)
import Legend.Continuous as Continuous
import Legend.Legend as Legend
import Color

main =
  let
      bins = Legend.toBins [0, 1, 2] [Color.red, Color.green, Color.blue]
  in
  Continuous.view bins (String.fromInt)
