module Domain.Participant exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Participant =
    { id : Int
    , name : String
    , nameLowercase : String -- used for case-insensitive sorting
    }


new : Int -> String -> Participant
new id name =
    { id = id
    , name = name
    , nameLowercase = name |> String.toLower
    }


decoder : Decoder Participant
decoder =
    Decode.map2
        new
        -- ID
        (Decode.field "i" Decode.int)
        -- name
        (Decode.field "n" Decode.string)


encode : Participant -> Value
encode participant =
    [ ( "i", participant.id |> Encode.int )
    , ( "n", participant.name |> Encode.string )
    ]
        |> Encode.object
