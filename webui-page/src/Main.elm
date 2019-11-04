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
    , theme : Theme
    , subsSelected : Int
    , subsCount : Int
    , audiosSelected : Int
    , audiosCount : Int
    , page : Page
    }


type Page
    = Home
    | Playlist


type ThemeSelect
    = WhiteTheme
    | BlackTheme
    | ColorfulTheme


type Msg
    = Sent (Result Http.Error D.Value)
    | TogglePause
    | SeekBackward
    | SeekForward
    | ChapterPrev
    | ChapterNext
    | PlaylistPrev
    | PlaylistNext
    | SubNext
    | AudioNext
    | SubDelay Float
    | AudioDelay Float
    | Fullscreen
    | AudioDeviceNext
    | PositionMsg Slider.Msg
    | VolumeMsg Slider.Msg
    | GetPositionElement (Result Browser.Dom.Error Browser.Dom.Element)
    | GetVolumeElement (Result Browser.Dom.Error Browser.Dom.Element)
    | GotStatus (Result Http.Error Status)
    | TogglePlaylist
    | PlaylistJump Int
    | SelectTheme ThemeSelect
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
    { status = initStatus
    , position = 0
    , maybePositionElement = Nothing
    , positionPointerDown = False
    , volume = 0
    , maybeVolumeElement = Nothing
    , volumePointerDown = False
    , theme = themeWhite
    , subsSelected = 0
    , subsCount = 0
    , audiosSelected = 0
    , audiosCount = 0
    , page = Home
    }


view model =
    { title = "Title"
    , body =
        [ layoutWith { options = [ focusStyle focusStyle_ ] }
            [ paddingEach { top = 10, right = 40, bottom = 40, left = 40 }, Background.color model.theme.backgroundColor ]
            (case model.page of
                Home ->
                    home model

                Playlist ->
                    playlist model
            )
        ]
    }


home model =
    column [ width fill, spacing 20 ]
        [ el [ width fill ] (el [ alignRight ] (buttonPlaylist model.theme))
        , paragraph
            [ Font.color model.theme.color
            , Font.size 40
            , Html.Attributes.style "word-break" "break-all" |> htmlAttribute
            ]
            [ text model.status.filename ]
        , el
            [ Font.color model.theme.color
            , Font.size model.theme.smallTextSize
            ]
            (text ("Sub-delay: " ++ String.fromInt model.status.subDelay ++ " ms"))
        , el
            [ Font.color model.theme.color
            , Font.size model.theme.smallTextSize
            ]
            (text ("Audio-delay: " ++ String.fromInt model.status.audioDelay ++ " ms"))
        , row [ width fill ]
            [ el
                [ Font.color model.theme.color
                , Font.size model.theme.smallTextSize
                , width fill
                ]
                (text ("-" ++ formatTime model.status.remaining))
            , el
                [ Font.color model.theme.color
                , Font.size model.theme.smallTextSize
                , width fill
                ]
                (el
                    [ centerX ]
                    (text (formatTime model.status.position))
                )
            , el
                [ Font.color model.theme.color
                , Font.size model.theme.smallTextSize
                , width fill
                ]
                (el
                    [ alignRight ]
                    (text (formatTime model.status.duration))
                )
            ]
        , Slider.view positionId
            model.positionPointerDown
            model.theme
            model.maybePositionElement
            model.position
            |> map PositionMsg
        , row [ width fill ]
            [ el
                [ Font.color model.theme.color
                , Font.size model.theme.smallTextSize
                , width fill
                ]
                (el [ width (px 50), height (px 50) ] (icon model.theme Icon.volumeDown))
            , el
                [ Font.color model.theme.color
                , Font.size model.theme.smallTextSize
                , width fill
                ]
                (el
                    [ centerX ]
                    (text (String.fromInt model.status.volume ++ "%"))
                )
            , el
                [ Font.color model.theme.color
                , Font.size model.theme.smallTextSize
                , width fill
                ]
                (el [ alignRight, width (px 50), height (px 50) ] (icon model.theme Icon.volumeUp))
            ]
        , Slider.view volumeId
            model.volumePointerDown
            model.theme
            model.maybeVolumeElement
            model.volume
            |> map VolumeMsg
        , button (Just TogglePause)
            model.theme
            (icon model.theme
                (if model.status.pause then
                    Icon.play

                 else
                    Icon.pause
                )
            )
        , row [ spacing 20, width fill ]
            [ button (Just SeekBackward)
                model.theme
                (icon model.theme Icon.backward)
            , button (Just SeekForward)
                model.theme
                (icon model.theme Icon.forward)
            ]
        , row [ spacing 20, width fill ]
            [ button (Just ChapterPrev)
                model.theme
                (icon model.theme Icon.stepBackward)
            , button (Just ChapterNext)
                model.theme
                (icon model.theme Icon.stepForward)
            ]
        , row [ spacing 20, width fill ]
            [ button (Just PlaylistPrev)
                model.theme
                (icon model.theme Icon.fastBackward)
            , button (Just PlaylistNext)
                model.theme
                (icon model.theme Icon.fastForward)
            ]
        , row [ spacing 20, width fill ]
            [ buttonText (Just SubNext)
                model.theme
                (text ("Next sub " ++ String.fromInt model.subsSelected ++ "/" ++ String.fromInt model.subsCount))
            , buttonText (Just AudioNext)
                model.theme
                (text ("Next audio " ++ String.fromInt model.audiosSelected ++ "/" ++ String.fromInt model.audiosCount))
            ]
        , row [ spacing 20, width fill ]
            [ buttonText (Just (SubDelay -0.05))
                model.theme
                (text "Sub delay -")
            , buttonText (Just (SubDelay 0.05))
                model.theme
                (text "Sub delay +")
            ]
        , row [ spacing 20, width fill ]
            [ buttonText (Just (AudioDelay -0.05))
                model.theme
                (text "Audio delay -")
            , buttonText (Just (AudioDelay 0.05))
                model.theme
                (text "Audio delay +")
            ]
        , row [ spacing 20, width fill ]
            [ buttonText (Just Fullscreen)
                model.theme
                (text
                    ("Fullscreen "
                        ++ (if model.status.fullscreen then
                                "off"

                            else
                                "on"
                           )
                    )
                )

            -- disabled because breaks on Ubuntu
            , buttonText Nothing
                model.theme
                (text "Audio device")
            ]
        , row [ width fill, spacing 20 ]
            [ buttonText (Just (SelectTheme WhiteTheme))
                model.theme
                (text "white")
            , buttonText (Just (SelectTheme BlackTheme))
                model.theme
                (text "black")
            , buttonText (Just (SelectTheme ColorfulTheme))
                model.theme
                (text "colorful")
            ]
        ]


playlist : Model -> Element Msg
playlist model =
    column [ width fill, spacing 20 ]
        [ row [ spacing 20, width fill ]
            [ buttonText (Just TogglePlaylist)
                model.theme
                (text "Hide")
            , button (Just TogglePause)
                model.theme
                (icon model.theme
                    (if model.status.pause then
                        Icon.play

                     else
                        Icon.pause
                    )
                )
            ]
        , column [ width fill, spacing 20 ] <|
            List.map
                (\( i, { filename, current } ) ->
                    buttonText (Just (PlaylistJump i)) model.theme <|
                        row
                            [ paddingEach { top = 20, right = 100, bottom = 20, left = 20 }
                            , width fill
                            ]
                            [ el [ width (px 80) ]
                                (el [ width (px 40), height (px 40) ]
                                    (if current then
                                        icon model.theme
                                            (if model.status.pause then
                                                Icon.pause

                                             else
                                                Icon.play
                                            )

                                     else
                                        none
                                    )
                                )
                            , paragraph
                                [ Html.Attributes.style "word-break" "break-all" |> htmlAttribute
                                , Font.size 30
                                ]
                                [ text <|
                                    Maybe.withDefault "" <|
                                        List.head <|
                                            List.reverse <|
                                                String.split "/" filename
                                ]
                            ]
                )
            <|
                List.indexedMap Tuple.pair model.status.playList
        ]


icon theme i =
    Icon.viewStyled [ colorToRgbaAttr theme.color ] i |> html


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


themeWhite =
    { borderWidth = 3
    , borderRounded = 6
    , borderColor = rgb255 0 0 0
    , buttonHeight = px 160
    , backgroundColor = rgb255 255 255 255
    , color = rgb255 0 0 0
    , smallTextSize = 36
    }


themeBlack =
    { borderWidth = 3
    , borderRounded = 6
    , borderColor = rgb255 255 255 255
    , buttonHeight = px 160
    , backgroundColor = rgb255 0 0 0
    , color = rgb255 255 255 255
    , smallTextSize = 36
    }


buttonPlaylist theme =
    Input.button
        [ Background.color theme.backgroundColor
        , height theme.buttonHeight
        ]
        { onPress = Just TogglePlaylist
        , label = el [ centerX, width (px 80), height (px 80) ] (icon theme Icon.listUl)
        }


button onPress theme element =
    Input.button
        [ Border.color theme.borderColor
        , Border.width theme.borderWidth
        , Border.rounded theme.borderRounded
        , Background.color theme.backgroundColor
        , padding 10
        , width fill
        , height theme.buttonHeight
        , Font.size 60
        ]
        { onPress = onPress
        , label = el [ centerX, width (px 80), height (px 80) ] element
        }


buttonText onPress theme element =
    Input.button
        [ Border.color theme.borderColor
        , Border.width theme.borderWidth
        , Border.rounded theme.borderRounded
        , Background.color theme.backgroundColor
        , padding 10
        , width fill
        , height theme.buttonHeight
        , Font.size 40
        ]
        { onPress = onPress
        , label = el [ width fill, Font.center, Font.color theme.color, Font.bold ] element
        }


formatTime seconds =
    let
        hour =
            seconds // 3600

        minute =
            (seconds - hour * 3600) // 60

        second =
            (seconds - hour * 3600) - minute * 60
    in
    String.pad 2 '0' (String.fromInt hour) ++ ":" ++ String.pad 2 '0' (String.fromInt minute) ++ ":" ++ String.pad 2 '0' (String.fromInt second)


update msg model =
    -- case Debug.log "msg" msg of
    case msg of
        Sent _ ->
            ( model, Cmd.none )

        TogglePause ->
            ( model, send "toggle_pause" )

        SeekBackward ->
            ( model, send "seek/-10" )

        SeekForward ->
            ( model, send "seek/10" )

        ChapterPrev ->
            ( model, send "add_chapter/-1" )

        ChapterNext ->
            ( model, send "add_chapter/1" )

        PlaylistPrev ->
            ( model, send "playlist_prev" )

        PlaylistNext ->
            ( model, send "playlist_next" )

        SubNext ->
            ( model, send "cycle_sub" )

        AudioNext ->
            ( model, send "cycle_audio" )

        SubDelay value ->
            ( model, send ("add_sub_delay/" ++ String.fromFloat value) )

        AudioDelay value ->
            ( model, send ("add_audio_delay/" ++ String.fromFloat value) )

        Fullscreen ->
            ( model, send "fullscreen" )

        AudioDeviceNext ->
            ( model, send "cycle_audio_device" )

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
                    ( { model | positionPointerDown = True, position = position }
                    , send ("set_position/" ++ String.fromFloat ((toFloat position / 100) * toFloat model.status.duration))
                    )

                Slider.PointerMoveMsg coords ->
                    let
                        position =
                            case model.maybePositionElement of
                                Just element ->
                                    round <| 100 * toFloat coords.x / element.element.width

                                Nothing ->
                                    0
                    in
                    ( { model | position = position }
                    , send ("set_position/" ++ String.fromFloat ((toFloat position / 100) * toFloat model.status.duration))
                    )

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
                    ( { model | volumePointerDown = True, volume = volume }
                    , send ("set_volume/" ++ String.fromFloat ((toFloat volume / 100) * toFloat model.status.volumeMax))
                    )

                Slider.PointerMoveMsg coords ->
                    let
                        volume =
                            case model.maybeVolumeElement of
                                Just element ->
                                    round <| 100 * toFloat coords.x / element.element.width

                                Nothing ->
                                    0
                    in
                    ( { model | volume = volume }
                    , send ("set_volume/" ++ String.fromFloat ((toFloat volume / 100) * toFloat model.status.volumeMax))
                    )

                Slider.PointerUpMsg ->
                    ( { model | volumePointerDown = False }, Cmd.none )

        GetPositionElement result ->
            ( { model | maybePositionElement = Result.toMaybe result }, Cmd.none )

        GetVolumeElement result ->
            ( { model | maybeVolumeElement = Result.toMaybe result }, Cmd.none )

        GotStatus (Ok newStatus) ->
            let
                status =
                    if model.positionPointerDown then
                        { newStatus | position = round ((toFloat model.position / 100) * toFloat newStatus.duration) }

                    else if model.volumePointerDown then
                        { newStatus | volume = round ((toFloat model.volume / 100) * toFloat newStatus.volumeMax) }

                    else
                        newStatus

                { subsSelected, subsCount, audiosSelected, audiosCount } =
                    List.foldl
                        (\{ id, type_, selected } acc ->
                            if type_ == "sub" then
                                { subsSelected =
                                    if selected then
                                        id

                                    else
                                        acc.subsSelected
                                , subsCount = acc.subsCount + 1
                                , audiosSelected = acc.audiosSelected
                                , audiosCount = acc.audiosCount
                                }

                            else if type_ == "audio" then
                                { subsSelected = acc.subsSelected
                                , subsCount = acc.subsCount
                                , audiosSelected =
                                    if selected then
                                        id

                                    else
                                        acc.audiosSelected
                                , audiosCount = acc.audiosCount + 1
                                }

                            else
                                acc
                        )
                        { subsSelected = 0, subsCount = 0, audiosSelected = 0, audiosCount = 0 }
                        status.trackList
            in
            ( { model
                | position = round (100 * toFloat status.position / toFloat status.duration)
                , volume = round (100 * toFloat status.volume / toFloat status.volumeMax)
                , status = status
                , subsSelected = subsSelected
                , subsCount = subsCount
                , audiosSelected = audiosSelected
                , audiosCount = audiosCount
              }
            , Cmd.none
            )

        GotStatus (Err err) ->
            ( model, Cmd.none )

        TogglePlaylist ->
            ( { model
                | page =
                    if model.page == Playlist then
                        Home

                    else
                        Playlist
              }
            , Cmd.none
            )

        PlaylistJump i ->
            ( model
            , Cmd.batch
                [ send ("playlist_jump/" ++ String.fromInt i)
                , send "play"
                ]
            )

        SelectTheme theme ->
            ( { model
                | theme =
                    case theme of
                        WhiteTheme ->
                            themeWhite

                        BlackTheme ->
                            themeBlack

                        ColorfulTheme ->
                            themeWhite
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
