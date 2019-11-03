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
    , smallTextSize : Int
    }


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Status =
    { duration : Int
    , position : Int
    , remaining : Int
    , pause : Bool
    , volume : Int
    , volumeMax : Int
    , filename : String
    , subDelay : Int
    , audioDelay : Int
    }


andMap =
    D.map2 (|>)


statusDecoder =
    D.succeed Status
        |> andMap (D.field "duration" D.int)
        |> andMap (D.field "position" D.int)
        |> andMap (D.field "remaining" D.int)
        |> andMap (D.field "pause" D.bool)
        |> andMap (D.field "volume" D.int)
        |> andMap (D.field "volume-max" D.int)
        |> andMap (D.field "filename" D.string)
        |> andMap (D.field "sub-delay" D.int)
        |> andMap (D.field "audio-delay" D.int)


initStatus =
    { duration = 0
    , position = 0
    , remaining = 0
    , pause = True
    , volume = 0
    , volumeMax = 0
    , filename = ""
    , subDelay = 0
    , audioDelay = 0
    }
