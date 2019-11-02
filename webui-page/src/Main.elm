module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Http
import Json.Decode as D exposing (Decoder)


type alias Model =
    { slider : Int
    }


type Msg
    = Sent (Result Http.Error D.Value)
    | TogglePause
    | SeekBack
    | SeekForward
    | PlaylistPrev
    | PlaylistNext
    | ClickMsg Coords


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    { slider = 0
    }


view model =
    { title = "Title"
    , body =
        [ Element.layoutWith { options = [ focusStyle focusStyle_ ] }
            [ padding 20 ]
            (column [ width fill, spacing 20 ]
                [ slider model.slider ClickMsg
                , button (Just TogglePause) "|> / ||"
                , row [ spacing 20, width fill ]
                    [ button (Just SeekBack) "<<"
                    , button (Just SeekForward) ">>"
                    ]
                , row [ spacing 20, width fill ]
                    [ button (Just PlaylistPrev) "|<<"
                    , button (Just PlaylistNext) ">>|"
                    ]
                ]
            )
        ]
    }


focusStyle_ : FocusStyle
focusStyle_ =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


button onPress text_ =
    Input.button
        [ Border.color (rgb255 0 0 0)
        , Border.width 3
        , Border.rounded 6
        , padding 10
        , width fill
        , height (px 160)
        , Font.size 60
        ]
        { onPress = onPress
        , label = el [ centerX ] (text text_)
        }


slider value msg =
    el [ width fill ]
        (el
            [ onClickCoords msg
            , width fill
            , height (px 160)
            , Border.color (rgb255 0 0 0)
            , Border.width 3
            , Border.rounded 6
            ]
            (el
                [ width (px value)
                , height fill
                , Background.color (rgb255 0 0 0)
                ]
                Element.none
            )
        )


onClickCoords : (Coords -> msg) -> Attribute msg
onClickCoords msg =
    Html.Events.on "click" (D.map msg localCoords) |> Element.htmlAttribute


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
            ( model, send "toggle_pause" )

        SeekBack ->
            ( model, send "seek/-10" )

        SeekForward ->
            ( model, send "seek/10" )

        PlaylistPrev ->
            ( model, send "playlist_prev" )

        PlaylistNext ->
            ( model, send "playlist_next" )

        ClickMsg coords ->
            ( { model | slider = coords.x }, Cmd.none )


send command =
    Http.post
        { url = "http://192.168.0.10:8080/api/" ++ command
        , body = Http.emptyBody
        , expect = Http.expectJson Sent D.value
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
