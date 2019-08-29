module Legend.Continuous exposing (..)

import Chroma.Chroma as Chroma
import Chroma.Converter.Out.ToHex as ChromaToHex
import Chroma.Types as ChromaTypes
import Html
import Html.Attributes as HtmlAttributes
import Legend.Legend as Legend
import List.Nonempty as Nonempty
import Svg
import Svg.Attributes as SvgAttributes


numberOfStops : Int
numberOfStops =
    102


segmentWidth : Int
segmentWidth =
    2


padding : Int
padding =
    26


svgHeight : Float
svgHeight =
    46.0


unitsYTranslate : Float
unitsYTranslate =
    34.0


textTopPadding : Int
textTopPadding =
    14


segmentHeight : Int
segmentHeight =
    24


colorYTranslate : Float
colorYTranslate =
    4.0


tickWidth : Int
tickWidth =
    1


view : List (Legend.Bin a) -> (a -> String) -> Html.Html msg
view bins show =
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
                    [ viewBody nonEmptyBins show
                    ]
                ]

        Nothing ->
            Html.div [ HtmlAttributes.class "legend" ]
                [ Html.text "No legend data"
                ]


viewBody : Nonempty.Nonempty (Legend.Bin a) -> (a -> String) -> Svg.Svg msg
viewBody bins show =
    let
        colours =
            Nonempty.map (\b -> ChromaTypes.RGBAColor b.color) bins

        values =
            Nonempty.map (\b -> show b.value) bins
    in
    Svg.g
        [ SvgAttributes.transform <| "translate(0, " ++ String.fromFloat colorYTranslate ++ ")" ]
        (viewColourBand colours
            ++ viewTicks values
        )


viewColourBand : Nonempty.Nonempty ChromaTypes.ExtColor -> List (Svg.Svg msg)
viewColourBand colours =
    let
        ( _, f ) =
            Chroma.domain (Nonempty.Nonempty 0 [ toFloat numberOfStops ]) colours
    in
    viewStopColor f 0
        :: List.map (\i -> viewStopColor f i) (List.range 1 numberOfStops)


viewTicks : Nonempty.Nonempty String -> List (Svg.Svg msg)
viewTicks values =
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
    List.indexedMap (\index tick -> viewTick (tickPosition index) tick) ticks
        ++ [ viewLabel numberOfStops max ]


viewTick : Float -> String -> Svg.Svg msg
viewTick position label =
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
        , viewLabel (floor position) label
        ]


viewStopColor : (Float -> ChromaTypes.ExtColor) -> Int -> Svg.Svg msg
viewStopColor f index =
    Svg.rect
        [ SvgAttributes.x << String.fromInt <| ((index + 1) * segmentWidth + padding)
        , SvgAttributes.y "0"
        , SvgAttributes.width <| String.fromInt segmentWidth
        , SvgAttributes.height <| String.fromInt segmentHeight
        , SvgAttributes.fill (f (toFloat index) |> ChromaToHex.toHex)
        ]
        []


viewLabel : Int -> String -> Svg.Svg msg
viewLabel index label =
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
