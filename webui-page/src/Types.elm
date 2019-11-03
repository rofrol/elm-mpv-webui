module Types exposing (..)

import Element exposing (Color, Length)
import Json.Decode as D


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


type alias Status =
    { duration : Int
    , position : Int
    , pause : Bool
    , volume : Int
    , volumeMax : Int
    , filename : String
    , subDelay : Int
    , audioDelay : Int
    }


statusDecoder =
    D.map8 Status
        (D.field "duration" D.int)
        (D.field "position" D.int)
        (D.field "pause" D.bool)
        (D.field "volume" D.int)
        (D.field "volume-max" D.int)
        (D.field "filename" D.string)
        (D.field "sub-delay" D.int)
        (D.field "audio-delay" D.int)


initStatus =
    { duration = 0
    , position = 0
    , pause = True
    , volume = 0
    , volumeMax = 0
    , filename = ""
    , subDelay = 0
    , audioDelay = 0
    }
