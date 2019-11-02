module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Input as Input
import Http
import Json.Decode as D


type Msg
    = Sent (Result Http.Error D.Value)
    | TogglePause


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
        [ Element.layout []
            (column []
                [ text "Hello"
                , Input.button []
                    { onPress = Just TogglePause
                    , label = text "play/pause"
                    }
                ]
            )
        ]
    }


update msg model =
    case msg of
        Sent _ ->
            ( model, Cmd.none )

        TogglePause ->
            ( model, send "toggle_pause" )


send command =
    Http.post
        { url = "http://192.168.0.10:8080/api/" ++ command
        , body = Http.emptyBody
        , expect = Http.expectJson Sent D.value
        }
