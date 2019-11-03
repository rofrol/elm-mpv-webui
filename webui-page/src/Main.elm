module Main exposing (..)

import Browser
import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import Html.Attributes
import Http
import Json.Decode as D
import Slider
import Task
import Time
import Types exposing (..)


type alias Model =
    { status : Status
    , position : Int
    , maybePositionElement : Maybe Browser.Dom.Element
    , positionPointerDown : Bool
    , volume : Int
    , maybeVolumeElement : Maybe Browser.Dom.Element
    , volumePointerDown : Bool
    , dark : Bool
    , style : Style
    }


type Msg
    = Sent (Result Http.Error D.Value)
    | TogglePause
    | SeekBack
    | SeekForward
    | PlaylistPrev
    | PlaylistNext
    | PositionMsg Slider.Msg
    | VolumeMsg Slider.Msg
    | GetPositionElement (Result Browser.Dom.Error Browser.Dom.Element)
    | GetVolumeElement (Result Browser.Dom.Error Browser.Dom.Element)
    | GotStatus (Result Http.Error Status)
    | ToggleDark
    | Tick Time.Posix


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
        [ Task.attempt GetPositionElement (Browser.Dom.getElement positionId)
        , Task.attempt GetVolumeElement (Browser.Dom.getElement volumeId)
        , getStatus
        ]
    )


positionId =
    "position"


volumeId =
    "volume"


initialModel : Model
initialModel =
    { status =
        { duration = 0
        , position = 0
        , pause = True
        , volume = 0
        , volumeMax = 0
        , filename = ""
        }
    , position = 0
    , maybePositionElement = Nothing
    , positionPointerDown = False
    , volume = 0
    , maybeVolumeElement = Nothing
    , volumePointerDown = False
    , dark = True
    , style = styleDark
    }


view model =
    { title = "Title"
    , body =
        [ Element.layoutWith { options = [ focusStyle focusStyle_ ] }
            [ padding 40, Background.color model.style.backgroundColor ]
            (column [ width fill, spacing 20 ]
                [ paragraph [ Font.color model.style.color, Font.size 40 ] [ text model.status.filename ]
                , Slider.view positionId
                    model.positionPointerDown
                    model.style
                    model.maybePositionElement
                    model.position
                    |> Element.map PositionMsg
                , Slider.view volumeId
                    model.volumePointerDown
                    model.style
                    model.maybeVolumeElement
                    model.volume
                    |> Element.map VolumeMsg
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
    Icon.viewStyled [ colorToRgbaAttr style.color ] i |> Element.html


colorToRgbaAttr color =
    let
        { red, green, blue, alpha } =
            toRgb color

        rgb =
            [ red, green, blue ] |> List.map ((*) 255 >> String.fromFloat) |> List.intersperse ", " |> List.foldl (++) ""
    in
    Html.Attributes.style "color" ("rgba(" ++ rgb ++ "," ++ String.fromFloat alpha ++ ")")


focusStyle_ : FocusStyle
focusStyle_ =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
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


update msg model =
    case Debug.log "msg" msg of
        Sent _ ->
            ( model, Cmd.none )

        TogglePause ->
            ( model, send "toggle_pause" )

        SeekBack ->
            ( model, send "seek/-10" )

        SeekForward ->
            ( model, send "seek/10" )

        PlaylistPrev ->
            ( model, send "playlist_prev" )

        PlaylistNext ->
            ( model, send "playlist_next" )

        PositionMsg subMsg ->
            case subMsg of
                Slider.PointerDownMsg coords ->
                    let
                        position =
                            case model.maybePositionElement of
                                Just element ->
                                    round <| 100 * toFloat coords.x / element.element.width

                                Nothing ->
                                    0
                    in
                    ( { model | positionPointerDown = True, position = position }, send ("set_position/" ++ String.fromFloat ((toFloat position / 100) * toFloat model.status.duration)) )

                Slider.PointerMoveMsg coords ->
                    let
                        position =
                            case model.maybePositionElement of
                                Just element ->
                                    round <| 100 * toFloat coords.x / element.element.width

                                Nothing ->
                                    0
                    in
                    ( { model | position = position }, send ("set_position/" ++ String.fromFloat ((toFloat position / 100) * toFloat model.status.duration)) )

                Slider.PointerUpMsg ->
                    ( { model | positionPointerDown = False }, Cmd.none )

        VolumeMsg subMsg ->
            case subMsg of
                Slider.PointerDownMsg coords ->
                    let
                        volume =
                            case model.maybeVolumeElement of
                                Just element ->
                                    round <| 100 * toFloat coords.x / element.element.width

                                Nothing ->
                                    0
                    in
                    ( { model | volumePointerDown = True, volume = volume }, send ("set_volume/" ++ String.fromFloat ((toFloat volume / 100) * toFloat model.status.volumeMax)) )

                Slider.PointerMoveMsg coords ->
                    let
                        volume =
                            case model.maybeVolumeElement of
                                Just element ->
                                    round <| 100 * toFloat coords.x / element.element.width

                                Nothing ->
                                    0
                    in
                    ( { model | volume = volume }, send ("set_volume/" ++ String.fromFloat ((toFloat volume / 100) * toFloat model.status.volumeMax)) )

                Slider.PointerUpMsg ->
                    ( { model | volumePointerDown = False }, Cmd.none )

        GetPositionElement result ->
            ( { model | maybePositionElement = Result.toMaybe result }, Cmd.none )

        GetVolumeElement result ->
            ( { model | maybeVolumeElement = Result.toMaybe result }, Cmd.none )

        GotStatus (Ok status) ->
            ( { model
                | position = round (100 * toFloat status.position / toFloat status.duration)
                , volume = round (100 * toFloat status.volume / toFloat status.volumeMax)
                , status = status
              }
            , Cmd.none
            )

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

        Tick _ ->
            ( model, getStatus )


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
    , volume : Int
    , volumeMax : Int
    , filename : String
    }


statusDecoder =
    D.map6 Status
        (D.field "duration" D.int)
        (D.field "position" D.int)
        (D.field "pause" D.bool)
        (D.field "volume" D.int)
        (D.field "volume-max" D.int)
        (D.field "filename" D.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
