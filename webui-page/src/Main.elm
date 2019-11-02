module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as D


type Msg
    = Sent (Result Http.Error D.Value)
    | TogglePause
    | SeekBack
    | SeekForward
    | PlaylistPrev
    | PlaylistNext


main : Program () () Msg
main =
    Browser.document
        { init = \_ -> ( (), Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view _ =
    { title = "Title"
    , body =
        [ Element.layoutWith { options = [ focusStyle focusStyle_ ] }
            [ padding 20 ]
            (column [ width fill, spacing 20 ]
                [ button (Just TogglePause) "|> / ||"
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
        , Border.width 2
        , Border.rounded 6
        , padding 10
        , width fill
        , height (px 160)
        , Font.size 60
        ]
        { onPress = onPress
        , label = el [ centerX ] (text text_)
        }


update msg model =
    case msg of
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


send command =
    Http.post
        { url = "http://192.168.0.10:8080/api/" ++ command
        , body = Http.emptyBody
        , expect = Http.expectJson Sent D.value
        }
