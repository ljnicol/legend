module Legend.LegendTest exposing (..)

import Expect as Expect
import Legend.Continuous as Continuous
import Test as Test


tests : Test.Test
tests =
    Test.describe "Legend API"
        [ testTickPosition ]


testTickPosition : Test.Test
testTickPosition =
    Test.describe "Tick Position"
        [ Test.test "Continuous position 0" <|
            \_ ->
                Continuous.tickPosition 102 2 0 |> Expect.equal 0.0
        , Test.test "Continuous position 1" <|
            \_ ->
                Continuous.tickPosition 102 2 1 |> Expect.equal 51
        ]
