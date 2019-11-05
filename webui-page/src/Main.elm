module Main exposing (..)

import Browser
import Element exposing (..)


main : Program () () msg
main =
    Browser.document
        { init = \_ -> ( (), Cmd.none )
        , view = view
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view _ =
    { title = "Title"
    , body =
        [ Element.layout [] (text "Hello") ]
    }
