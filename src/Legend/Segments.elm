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
    { numberOfStops = numberOfBins - 1
    , numberOfTicks = numberOfBins
    , segmentWidth = 50
    , padding = 26
    , svgHeight = 500
    , unitsYTranslate = 34.0
    , textTopPadding = 14
    , segmentHeight = 26
    , colorYTranslate = 4.0
    , tickWidth = 2
    , direction = Legend.Horizontal
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
viewBody bins show ({ colorYTranslate, numberOfStops } as config) =
    let
        first =
            Nonempty.head bins

        last =
            List.head (List.reverse (Nonempty.tail bins))

        rest =
            -- get the middle bit of the list
            Nonempty.tail bins
                |> List.reverse
                |> List.tail
                |> Maybe.map List.reverse
                |> Maybe.withDefault []

        lastLabel =
            case last of
                Just l ->
                    [ viewStopLabel config (numberOfStops - 1) (show l.value) ]

                Nothing ->
                    []

        colors =
            List.map (\b -> ChromaTypes.RGBAColor b.color) rest

        values =
            List.map (\b -> show b.value) rest
    in
    Svg.g [ SvgAttributes.transform <| "translate(0, " ++ String.fromFloat colorYTranslate ++ ")" ]
        (viewStop config -1 (show first.value) (ChromaTypes.RGBAColor first.color)
            :: Util.indexedMap2 (viewStop config) values colors
            ++ lastLabel
        )


viewStop : Legend.Config -> Int -> String -> ChromaTypes.ExtColor -> Svg.Svg msg
viewStop config index value color =
    Svg.g []
        [ viewStopColor config index color
        , viewStopLabel config index value
        ]


viewStopColor : Legend.Config -> Int -> ChromaTypes.ExtColor -> Svg.Svg msg
viewStopColor config index color =
    let
        directionalSetup =
            Legend.directionSetupColor index config
    in
    Svg.rect
        [ SvgAttributes.x directionalSetup.x
        , SvgAttributes.y directionalSetup.y
        , SvgAttributes.width directionalSetup.width
        , SvgAttributes.height directionalSetup.height
        , SvgAttributes.fill <| ChromaToHex.toHex color
        ]
        []


viewStopLabel : Legend.Config -> Int -> String -> Svg.Svg msg
viewStopLabel config index strLabel =
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
        [ Svg.text strLabel ]
