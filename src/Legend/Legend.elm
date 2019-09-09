module Legend.Legend exposing (Bin, Config, Direction(..), directionSetupColor, directionSetupLabel, directionSetupTicks, directionSetupView, toBin, toBins)

import Color
import Legend.Util as Util


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
    , direction : Direction
    }


type Direction
    = Horizontal
    | Vertical


type alias DirectionSetup =
    { x : String
    , y : String
    , width : String
    , height : String
    }


type alias DirectionSetupView =
    { width : String
    , height : String
    }


type alias DirectionSetupLabel =
    { x : String
    , y : String
    }


directionSetupView : Config -> DirectionSetupView
directionSetupView { numberOfStops, segmentWidth, padding, svgHeight, direction } =
    let
        totalSvgWidth =
            numberOfStops * segmentWidth + padding * 2
    in
    case direction of
        Horizontal ->
            { width = String.fromInt totalSvgWidth
            , height = String.fromFloat svgHeight
            }

        Vertical ->
            { width = String.fromFloat svgHeight
            , height = String.fromInt totalSvgWidth
            }


directionSetupColor : Int -> Config -> DirectionSetup
directionSetupColor index { direction, segmentHeight, segmentWidth, padding } =
    case direction of
        Vertical ->
            { x = String.fromInt padding
            , y = String.fromInt <| ((index + 1) * segmentWidth + padding)
            , width = String.fromInt segmentHeight
            , height = String.fromInt segmentWidth
            }

        Horizontal ->
            { x = String.fromInt <| ((index + 1) * segmentWidth + padding)
            , y = "0"
            , width = String.fromInt segmentWidth
            , height = String.fromInt segmentHeight
            }


directionSetupLabel : Int -> Config -> DirectionSetupLabel
directionSetupLabel index { direction, segmentHeight, segmentWidth, padding, textTopPadding } =
    case direction of
        Vertical ->
            { x = String.fromInt textTopPadding
            , y = String.fromInt <| (index + 1) * segmentWidth + padding
            }

        Horizontal ->
            { x = String.fromInt <| (index + 1) * segmentWidth + padding
            , y = String.fromInt <| segmentHeight + textTopPadding
            }


directionSetupTicks : Float -> Config -> DirectionSetup
directionSetupTicks position { direction, segmentHeight, segmentWidth, padding, tickWidth } =
    case direction of
        Vertical ->
            { x = String.fromInt padding
            , y = String.fromFloat <| ((position + 1) * toFloat segmentWidth + toFloat padding)
            , width = String.fromInt segmentHeight
            , height = String.fromInt tickWidth
            }

        Horizontal ->
            { x = String.fromFloat <| ((position + 1) * toFloat segmentWidth + toFloat padding)
            , y = "0"
            , width = String.fromInt tickWidth
            , height = String.fromInt segmentHeight
            }
