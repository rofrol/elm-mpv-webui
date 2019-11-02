module Main exposing (..)

import Browser
import Element exposing (..)
import Http
import Json.Decode as D


type Msg
    = Sent (Result Http.Error D.Value)


main : Program () () Msg
main =
    Browser.document
        { init =
            \_ ->
                ( ()
                , Http.post
                    { url = "http://192.168.0.10:8080/api/toggle_pause"
                    , body = Http.emptyBody
                    , expect = Http.expectJson Sent D.value
                    }
                )
        , view = view
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view _ =
    { title = "Title"
    , body =
        [ Element.layout [] (text "Hello") ]
    }
