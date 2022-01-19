module Domain.Participant exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Id =
    Int


idFromString : String -> Maybe Id
idFromString =
    String.toInt


idToString : Id -> String
idToString =
    String.fromInt


type alias Participant =
    { id : Id
    , name : String
    , nameLowercase : String -- used for case-insensitive sorting
    }


new : Id -> String -> Participant
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


type alias NameIndex =
    Dict Id String


lookupName : Id -> NameIndex -> String
lookupName id index =
    case Dict.get id index of
        Nothing ->
            "<" ++ idToString id ++ ">"

        Just name ->
            name
