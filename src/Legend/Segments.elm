module Legend.Segments exposing (view)

import Chroma.Chroma as Chroma
import Chroma.Converter.Out.ToHex as ChromaToHex
import Chroma.Types as ChromaTypes
import Html
import Html.Attributes as HtmlAttributes
import Legend.Legend as Legend
import Legend.Util as Util
import List.Nonempty as Nonempty
import Svg
import Svg.Attributes as SvgAttributes


defaultConfig : Int -> Legend.Config
defaultConfig numberOfBins =
    { numberOfStops = numberOfBins + 1
    , numberOfTicks = numberOfBins
    , segmentWidth = 50
    , padding = 26
    , svgHeight = 46.0
    , unitsYTranslate = 34.0
    , textTopPadding = 14
    , segmentHeight = 26
    , colorYTranslate = 4.0
    , tickWidth = 1
    }


view : List (Legend.Bin a) -> (a -> String) -> Html.Html msg
view bins show =
    case Nonempty.fromList bins of
        Just nonEmptyBins ->
            viewWithConfig nonEmptyBins show (defaultConfig <| List.length bins)

        Nothing ->
            Html.div [ HtmlAttributes.class "legend" ]
                [ Html.text "No legend data"
                ]


viewWithConfig : Nonempty.Nonempty (Legend.Bin a) -> (a -> String) -> Legend.Config -> Html.Html msg
viewWithConfig bins show ({ segmentWidth, padding, numberOfStops, colorYTranslate, svgHeight, unitsYTranslate } as config) =
    let
        svgWidth =
            numberOfStops
                * segmentWidth
                + padding
                * 2
                |> max 300

        colors =
            Nonempty.map (\b -> ChromaTypes.RGBAColor b.color) bins

        values =
            Nonempty.map (\b -> show b.value) bins

        first =
            3
    in
    Html.div [ HtmlAttributes.class "legend" ]
        [ Svg.svg
            [ SvgAttributes.width <| String.fromInt svgWidth
            , SvgAttributes.height <| String.fromFloat svgHeight
            , SvgAttributes.fill "white"
            ]
            [ Svg.g [ SvgAttributes.transform <| "translate(0, " ++ String.fromFloat colorYTranslate ++ ")" ]
                (viewStopColor config -1 (Nonempty.head colors)
                    :: Util.indexedMap2 (viewStop config) (Nonempty.toList values) (Nonempty.toList colors)
                 --                    ++ [ viewStopLabel config (numberOfStops - 1)  ]
                )
            ]
        ]


viewStop : Legend.Config -> Int -> String -> ChromaTypes.ExtColor -> Svg.Svg msg
viewStop config index value color =
    Svg.g []
        [ viewStopColor config index color
        , viewStopLabel config index value
        ]


viewStopColor : Legend.Config -> Int -> ChromaTypes.ExtColor -> Svg.Svg msg
viewStopColor { segmentWidth, segmentHeight, padding } index color =
    Svg.rect
        [ SvgAttributes.x << String.fromInt <| ((index + 1) * segmentWidth + padding)
        , SvgAttributes.y "0"
        , SvgAttributes.width <| String.fromInt segmentWidth
        , SvgAttributes.height <| String.fromInt segmentHeight
        , SvgAttributes.fill <| ChromaToHex.toHex color
        ]
        []


viewStopLabel : Legend.Config -> Int -> String -> Svg.Svg msg
viewStopLabel { segmentWidth, padding, textTopPadding, segmentHeight } index strLabel =
    Svg.text_
        [ SvgAttributes.x << String.fromInt <| (index + 1) * segmentWidth + padding
        , SvgAttributes.y << String.fromInt <| segmentHeight + textTopPadding
        , SvgAttributes.textAnchor "middle"
        , SvgAttributes.fill "black"
        ]
        [ Svg.text strLabel ]
