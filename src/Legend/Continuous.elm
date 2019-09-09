module Legend.Continuous exposing (tickPosition, view)

import Chroma.Chroma as Chroma
import Chroma.Converter.Out.ToHex as ChromaToHex
import Chroma.Types as ChromaTypes
import Html
import Html.Attributes as HtmlAttributes
import Legend.Legend as Legend
import List.Nonempty as Nonempty
import Svg
import Svg.Attributes as SvgAttributes


defaultConfig : Int -> Legend.Config
defaultConfig numberOfBins =
    { numberOfStops = 100
    , numberOfTicks = numberOfBins - 1
    , segmentWidth = 2
    , padding = 26
    , svgHeight = 50
    , unitsYTranslate = 34.0
    , textTopPadding = 14
    , segmentHeight = 24
    , colorYTranslate = 4.0
    , tickWidth = 1
    , direction = Legend.Vertical
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
viewWithConfig bins show config =
    let
        directionalSetup =
            Legend.directionSetupView config
    in
    Html.div [ HtmlAttributes.class "legend" ]
        [ Svg.svg
            [ SvgAttributes.width directionalSetup.width
            , SvgAttributes.height directionalSetup.height
            , SvgAttributes.fill "white"
            ]
            [ viewBody bins show config
            ]
        ]


viewBody : Nonempty.Nonempty (Legend.Bin a) -> (a -> String) -> Legend.Config -> Svg.Svg msg
viewBody bins show ({ colorYTranslate } as config) =
    let
        colors =
            Nonempty.map (\b -> ChromaTypes.RGBAColor b.color) bins

        values =
            Nonempty.map (\b -> show b.value) bins
    in
    Svg.g
        [ SvgAttributes.transform <| "translate(0, " ++ String.fromFloat colorYTranslate ++ ")" ]
        (viewColourBand colors config
            ++ viewTicks values config
        )


viewColourBand : Nonempty.Nonempty ChromaTypes.ExtColor -> Legend.Config -> List (Svg.Svg msg)
viewColourBand colours ({ numberOfStops } as config) =
    let
        chromaDomain =
            numberOfStops - 1

        ( _, f ) =
            Chroma.domain (Nonempty.Nonempty 0 [ toFloat chromaDomain ]) colours
    in
    viewStopColor f 0 config
        :: List.map (\i -> viewStopColor f i config) (List.range 1 chromaDomain)


viewTicks : Nonempty.Nonempty String -> Legend.Config -> List (Svg.Svg msg)
viewTicks values ({ numberOfStops, numberOfTicks } as config) =
    let
        reverseTicks =
            values |> Nonempty.reverse

        ticks =
            reverseTicks |> Nonempty.toList |> List.reverse

        max =
            reverseTicks |> Nonempty.head
    in
    List.indexedMap (\index tick -> viewTick (tickPosition numberOfStops numberOfTicks index) tick config) ticks
        ++ [ viewLabel numberOfStops max config ]


tickPosition : Int -> Int -> Int -> Float
tickPosition numberOfStops numberOfTicks index =
    toFloat numberOfStops * toFloat index / toFloat numberOfTicks


viewTick : Float -> String -> Legend.Config -> Svg.Svg msg
viewTick position label ({ segmentWidth, segmentHeight, tickWidth, padding, direction } as config) =
    let
        directionalSetup =
            Legend.directionSetupTicks position config
    in
    Svg.g []
        [ Svg.rect
            [ SvgAttributes.x directionalSetup.x
            , SvgAttributes.y directionalSetup.y
            , SvgAttributes.width directionalSetup.width
            , SvgAttributes.height directionalSetup.height
            , SvgAttributes.fill "white"
            ]
            []
        , viewLabel (floor position) label config
        ]


viewStopColor : (Float -> ChromaTypes.ExtColor) -> Int -> Legend.Config -> Svg.Svg msg
viewStopColor f index config =
    let
        directionalSetup =
            Legend.directionSetupColor index config
    in
    Svg.rect
        [ SvgAttributes.x directionalSetup.x
        , SvgAttributes.y directionalSetup.y
        , SvgAttributes.width directionalSetup.width
        , SvgAttributes.height directionalSetup.height
        , SvgAttributes.fill (f (toFloat index) |> ChromaToHex.toHex)
        ]
        []


viewLabel : Int -> String -> Legend.Config -> Svg.Svg msg
viewLabel index label config =
    let
        directionalSetup =
            Legend.directionSetupLabel index config
    in
    Svg.text_
        [ SvgAttributes.x directionalSetup.x
        , SvgAttributes.y directionalSetup.y
        , SvgAttributes.textAnchor "middle"
        , SvgAttributes.fill "black"
        ]
        [ Svg.text label ]
