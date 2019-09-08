module Tests exposing (all)

import Legend.LegendTest
import Test


all : Test.Test
all =
    Test.describe "Legend"
        [ Legend.LegendTest.tests ]
