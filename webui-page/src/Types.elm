module Types exposing (..)

import Element exposing (Color, Length)


type alias Style =
    { borderWidth : Int
    , borderRounded : Int
    , borderColor : Color
    , buttonHeight : Length
    , backgroundColor : Color
    , color : Color
    }


type alias Coords =
    { x : Int
    , y : Int
    }
