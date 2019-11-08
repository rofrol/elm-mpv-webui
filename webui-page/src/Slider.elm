module Slider exposing (..)

import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder)
import Types exposing (..)


type Msg
    = PointerDownMsg Coords
    | PointerMoveMsg Coords
    | PointerUpMsg Coords


view : String -> Bool -> Theme -> Maybe Browser.Dom.Element -> Int -> Element Msg
view id pointerDown theme maybePositionElement position =
    let
        value : Int
        value =
            case maybePositionElement of
                Just element ->
                    round <| toFloat position / 100 * element.element.width

                Nothing ->
                    0

        pointerAttrs =
            List.concat
                [ if pointerDown then
                    [ onPointerMoveCoords PointerMoveMsg
                    , onPointerUpCoords PointerUpMsg
                    ]

                  else
                    [ onPointerDownCoords PointerDownMsg ]
                ]
    in
    el
        [ width fill
        , height theme.buttonHeight
        , Border.color theme.borderColor
        , Border.width theme.sliderBorderWidth
        , Border.rounded theme.borderRounded
        ]
        (el [ width fill, height fill, Html.Attributes.id id |> Element.htmlAttribute ]
            (el
                ([ width fill
                 , height fill
                 ]
                    ++ pointerAttrs
                )
                (el
                    [ width (px value)
                    , height fill
                    , Background.color theme.borderColor
                    ]
                    Element.none
                )
            )
        )


onPointerDownCoords : (Coords -> msg) -> Attribute msg
onPointerDownCoords msg =
    Html.Events.on "pointerdown" (D.map msg localCoords) |> Element.htmlAttribute


onPointerMoveCoords : (Coords -> msg) -> Attribute msg
onPointerMoveCoords msg =
    Html.Events.on "pointermove" (D.map msg localCoords) |> Element.htmlAttribute


onPointerUpCoords : (Coords -> msg) -> Attribute msg
onPointerUpCoords msg =
    Html.Events.on "pointerup" (D.map msg localCoords) |> Element.htmlAttribute


localCoords : Decoder Coords
localCoords =
    D.map2 Coords
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)
