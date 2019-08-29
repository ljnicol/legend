module Legend.Continuous exposing (view)

import Chroma.Chroma as Chroma
import Chroma.Converter.Out.ToHex as ChromaToHex
import Chroma.Types as ChromaTypes
import Html
import Html.Attributes as HtmlAttributes
import Legend.Legend as Legend
import List.Nonempty as Nonempty
import Svg
import Svg.Attributes as SvgAttributes


type alias Config =
    { numberOfStops : Int
    , segmentWidth : Int
    , padding : Int
    , svgHeight : Float
    , unitsYTranslate : Float
    , textTopPadding : Int
    , segmentHeight : Int
    , colorYTranslate : Float
    , tickWidth : Int
    }


defaultConfig : Config
defaultConfig =
    { numberOfStops = 102
    , segmentWidth = 2
    , padding = 26
    , svgHeight = 46.0
    , unitsYTranslate = 34.0
    , textTopPadding = 14
    , segmentHeight = 24
    , colorYTranslate = 4.0
    , tickWidth = 1
    }


view : List (Legend.Bin a) -> (a -> String) -> Html.Html msg
view bins show =
    viewWithConfig bins show defaultConfig


viewWithConfig : List (Legend.Bin a) -> (a -> String) -> Config -> Html.Html msg
viewWithConfig bins show ({ numberOfStops, segmentWidth, padding, svgHeight } as config) =
    let
        -- 4 pixels wide * 102 -> 408
        totalSvgWidth =
            numberOfStops * segmentWidth + padding * 2
    in
    case Nonempty.fromList bins of
        Just nonEmptyBins ->
            Html.div [ HtmlAttributes.class "legend" ]
                [ Svg.svg
                    [ SvgAttributes.width <| String.fromInt totalSvgWidth
                    , SvgAttributes.height <| String.fromFloat svgHeight
                    , SvgAttributes.fill "white"
                    ]
                    [ viewBody nonEmptyBins show config
                    ]
                ]

        Nothing ->
            Html.div [ HtmlAttributes.class "legend" ]
                [ Html.text "No legend data"
                ]


viewBody : Nonempty.Nonempty (Legend.Bin a) -> (a -> String) -> Config -> Svg.Svg msg
viewBody bins show ({ colorYTranslate } as config) =
    let
        colours =
            Nonempty.map (\b -> ChromaTypes.RGBAColor b.color) bins

        values =
            Nonempty.map (\b -> show b.value) bins
    in
    Svg.g
        [ SvgAttributes.transform <| "translate(0, " ++ String.fromFloat colorYTranslate ++ ")" ]
        (viewColourBand colours config
            ++ viewTicks values config
        )


viewColourBand : Nonempty.Nonempty ChromaTypes.ExtColor -> Config -> List (Svg.Svg msg)
viewColourBand colours ({ numberOfStops } as config) =
    let
        ( _, f ) =
            Chroma.domain (Nonempty.Nonempty 0 [ toFloat numberOfStops ]) colours
    in
    viewStopColor f 0 config
        :: List.map (\i -> viewStopColor f i config) (List.range 1 numberOfStops)


viewTicks : Nonempty.Nonempty String -> Config -> List (Svg.Svg msg)
viewTicks values ({ numberOfStops } as config) =
    let
        numberOfTicks =
            List.length ticks

        tickPosition index =
            toFloat numberOfStops * toFloat index / toFloat numberOfTicks

        reverseTicks =
            values |> Nonempty.reverse

        ticks =
            reverseTicks |> Nonempty.tail |> List.reverse

        max =
            reverseTicks |> Nonempty.head
    in
    List.indexedMap (\index tick -> viewTick (tickPosition index) tick config) ticks
        ++ [ viewLabel numberOfStops max config ]


viewTick : Float -> String -> Config -> Svg.Svg msg
viewTick position label ({ segmentWidth, segmentHeight, tickWidth, padding } as config) =
    Svg.g []
        [ Svg.rect
            [ (SvgAttributes.x << String.fromFloat) <|
                ((position + 1) * toFloat segmentWidth + toFloat padding)
            , SvgAttributes.y "0"
            , SvgAttributes.width <| String.fromInt tickWidth
            , SvgAttributes.height <| String.fromInt segmentHeight
            , SvgAttributes.fill "white"
            ]
            []
        , viewLabel (floor position) label config
        ]


viewStopColor : (Float -> ChromaTypes.ExtColor) -> Int -> Config -> Svg.Svg msg
viewStopColor f index { segmentWidth, segmentHeight, tickWidth, padding } =
    Svg.rect
        [ SvgAttributes.x << String.fromInt <| ((index + 1) * segmentWidth + padding)
        , SvgAttributes.y "0"
        , SvgAttributes.width <| String.fromInt segmentWidth
        , SvgAttributes.height <| String.fromInt segmentHeight
        , SvgAttributes.fill (f (toFloat index) |> ChromaToHex.toHex)
        ]
        []


viewLabel : Int -> String -> Config -> Svg.Svg msg
viewLabel index label { segmentWidth, segmentHeight, tickWidth, padding, textTopPadding } =
    Svg.text_
        [ SvgAttributes.x << String.fromInt <| (index + 1) * segmentWidth + padding
        , SvgAttributes.y << String.fromInt <| segmentHeight + textTopPadding
        , SvgAttributes.textAnchor "middle"
        , SvgAttributes.fill "black"
        ]
        [ Svg.text label ]


viewTextElement : String -> Float -> Svg.Svg msg
viewTextElement string y =
    Svg.text_
        [ SvgAttributes.x "50%"
        , SvgAttributes.y <| String.fromFloat y
        , SvgAttributes.textAnchor "middle"
        , SvgAttributes.fill "black"
        ]
        [ Svg.text string ]
