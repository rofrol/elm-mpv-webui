module Types exposing (..)

import Element exposing (Color, Length)
import Json.Decode as D


andMap =
    D.map2 (|>)


type alias Theme =
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
    , trackList : List Track
    , playList : List Play
    , fullscreen : Bool
    }


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
        |> andMap (D.field "track-list" (D.list trackDecoder))
        |> andMap (D.field "playlist" (D.list playDecoder))
        |> andMap (D.field "fullscreen" D.bool)


type alias Track =
    { id : Int
    , type_ : String
    , selected : Bool
    }


trackDecoder =
    D.succeed Track
        |> andMap (D.field "id" D.int)
        |> andMap (D.field "type" D.string)
        |> andMap (D.field "selected" D.bool)


type alias Play =
    { filename : String
    , current : Bool
    }


playDecoder =
    D.succeed Play
        |> andMap (D.field "filename" D.string)
        |> andMap
            (D.maybe (D.field "current" D.bool)
                |> D.map (\maybeBool -> maybeBool |> Maybe.withDefault False)
            )


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
    , trackList = []
    , playList = []
    , fullscreen = False
    }
