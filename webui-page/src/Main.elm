module Main exposing (..)

import Browser
import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import Html.Attributes
import Html.Events
import Http
import Json.Decode as D exposing (Decoder)
import Task


type alias Model =
    { position : Int
    , maybePositionElement : Maybe Browser.Dom.Element
    , status : Status
    , dark : Bool
    , style : Style
    , mouseDown : Bool
    }


type Msg
    = Sent (Result Http.Error D.Value)
    | TogglePause
    | SeekBack
    | SeekForward
    | PlaylistPrev
    | PlaylistNext
    | MouseDownMsg Coords
    | MouseMoveMsg Coords
    | MouseUpMsg
    | GetPositionElement (Result Browser.Dom.Error Browser.Dom.Element)
    | GotStatus (Result Http.Error Status)
    | ToggleDark


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init _ =
    ( initialModel
    , Cmd.batch
        [ Task.attempt GetPositionElement (Browser.Dom.getElement "position")
        , getStatus
        ]
    )


initialModel : Model
initialModel =
    { position = 0
    , maybePositionElement = Nothing
    , status = { duration = 0, position = 0, pause = True }
    , dark = True
    , style = styleDark
    , mouseDown = False
    }


view model =
    { title = "Title"
    , body =
        [ Element.layoutWith { options = [ focusStyle focusStyle_ ] }
            [ padding 40, Background.color model.style.backgroundColor ]
            (column [ width fill, spacing 20 ]
                [ slider "position" model.mouseDown MouseDownMsg MouseMoveMsg MouseUpMsg model.style model.maybePositionElement model.position
                , button (Just TogglePause)
                    model.style
                    (icon model.style
                        (if model.status.pause then
                            Icon.play

                         else
                            Icon.pause
                        )
                    )
                , row [ spacing 20, width fill ]
                    [ button (Just SeekBack)
                        model.style
                        (icon model.style Icon.backward)
                    , button (Just SeekForward)
                        model.style
                        (icon model.style Icon.forward)
                    ]
                , row [ spacing 20, width fill ]
                    [ button (Just PlaylistPrev)
                        model.style
                        (icon model.style Icon.fastBackward)
                    , button (Just PlaylistNext)
                        model.style
                        (icon model.style Icon.fastForward)
                    ]
                , button (Just ToggleDark)
                    model.style
                    (icon model.style Icon.adjust)
                ]
            )
        ]
    }


icon style i =
    let
        { red, green, blue, alpha } =
            toRgb style.color

        rgb =
            [ red, green, blue ] |> List.map ((*) 255 >> String.fromFloat) |> List.intersperse ", " |> List.foldl (++) ""
    in
    Icon.viewStyled
        [ Html.Attributes.style "color" ("rgba(" ++ rgb ++ "," ++ String.fromFloat alpha ++ ")") ]
        i
        |> Element.html


focusStyle_ : FocusStyle
focusStyle_ =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


type alias Style =
    { borderWidth : Int
    , borderRounded : Int
    , borderColor : Color
    , buttonHeight : Length
    , backgroundColor : Color
    , color : Color
    }


styleLight =
    { borderWidth = 3
    , borderRounded = 6
    , borderColor = rgb255 0 0 0
    , buttonHeight = px 160
    , backgroundColor = rgb255 255 255 255
    , color = rgb255 0 0 0
    }


styleDark =
    { borderWidth = 3
    , borderRounded = 6
    , borderColor = rgb255 255 255 255
    , buttonHeight = px 160
    , backgroundColor = rgb255 0 0 0
    , color = rgb255 255 255 255
    }


button onPress style element =
    Input.button
        [ Border.color style.borderColor
        , Border.width style.borderWidth
        , Border.rounded style.borderRounded
        , Background.color style.backgroundColor
        , padding 10
        , width fill
        , height style.buttonHeight
        , Font.size 60
        ]
        { onPress = onPress
        , label = el [ centerX, width (px 80), height (px 80) ] element
        }


slider : String -> Bool -> (Coords -> Msg) -> (Coords -> Msg) -> Msg -> Style -> Maybe Browser.Dom.Element -> Int -> Element Msg
slider id mouseDown mouseDownMsg mouseMoveMsg mouseUpMsg style maybePositionElement position =
    let
        value : Int
        value =
            case maybePositionElement of
                Just element ->
                    round <| toFloat position / 100 * element.element.width

                Nothing ->
                    0

        mouseAttrs =
            List.concat
                [ if mouseDown then
                    [ onMouseMoveCoords mouseMoveMsg
                    , Events.onMouseUp mouseUpMsg
                    ]

                  else
                    [ onMouseDownCoords mouseDownMsg ]
                ]
    in
    el
        [ width fill
        , height style.buttonHeight
        , Border.color style.borderColor
        , Border.width style.borderWidth
        , Border.rounded style.borderRounded
        ]
        (el [ width fill, height fill, Html.Attributes.id id |> Element.htmlAttribute ]
            (el
                ([ width fill
                 , height fill
                 ]
                    ++ mouseAttrs
                )
                (el
                    [ width (px value)
                    , height fill
                    , Background.color style.borderColor
                    ]
                    Element.none
                )
            )
        )


onMouseDownCoords : (Coords -> msg) -> Attribute msg
onMouseDownCoords msg =
    Html.Events.on "mousedown" (D.map msg localCoords) |> Element.htmlAttribute


onMouseMoveCoords : (Coords -> msg) -> Attribute msg
onMouseMoveCoords msg =
    Html.Events.on "mousemove" (D.map msg localCoords) |> Element.htmlAttribute


type alias Coords =
    { x : Int
    , y : Int
    }


localCoords : Decoder Coords
localCoords =
    D.map2 Coords
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)


update msg model =
    case Debug.log "msg" msg of
        Sent _ ->
            ( model, Cmd.none )

        TogglePause ->
            let
                status =
                    model.status

                newStatus =
                    { status | pause = not status.pause }
            in
            ( { model | status = newStatus }, send "toggle_pause" )

        SeekBack ->
            ( model, send "seek/-10" )

        SeekForward ->
            ( model, send "seek/10" )

        PlaylistPrev ->
            ( model, send "playlist_prev" )

        PlaylistNext ->
            ( model, send "playlist_next" )

        MouseDownMsg coords ->
            let
                position =
                    case model.maybePositionElement of
                        Just element ->
                            round <| 100 * toFloat coords.x / element.element.width

                        Nothing ->
                            0
            in
            ( { model | mouseDown = True, position = position }, send ("set_position/" ++ String.fromFloat ((toFloat position / 100) * toFloat model.status.duration)) )

        MouseMoveMsg coords ->
            let
                position =
                    case model.maybePositionElement of
                        Just element ->
                            round <| 100 * toFloat coords.x / element.element.width

                        Nothing ->
                            0
            in
            ( { model | position = position }, send ("set_position/" ++ String.fromFloat ((toFloat position / 100) * toFloat model.status.duration)) )

        MouseUpMsg ->
            ( { model | mouseDown = False }, Cmd.none )

        GetPositionElement result ->
            ( { model | maybePositionElement = Result.toMaybe result }, Cmd.none )

        GotStatus (Ok status) ->
            ( { model | position = round (100 * toFloat status.position / toFloat status.duration), status = status }, Cmd.none )

        GotStatus (Err err) ->
            ( model, Cmd.none )

        ToggleDark ->
            let
                dark =
                    not model.dark
            in
            ( { model
                | dark = dark
                , style =
                    if dark then
                        styleDark

                    else
                        styleLight
              }
            , Cmd.none
            )


send command =
    Http.post
        { url = "http://192.168.0.10:8080/api/" ++ command
        , body = Http.emptyBody
        , expect = Http.expectJson Sent D.value
        }


getStatus =
    Http.get
        { url = "http://192.168.0.10:8080/api/status"
        , expect = Http.expectJson GotStatus statusDecoder
        }


type alias Status =
    { duration : Int
    , position : Int
    , pause : Bool
    }


statusDecoder =
    D.map3 Status
        (D.field "duration" D.int)
        (D.field "position" D.int)
        (D.field "pause" D.bool)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
