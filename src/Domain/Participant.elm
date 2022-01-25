module Domain.Participant exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List


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


type alias Index =
    Dict Id Int


lookup : Id -> Index -> List Participant -> Maybe Participant
lookup id idToIndex participants =
    idToIndex
        |> Dict.get id
        |> Maybe.andThen (\index -> participants |> List.getAt index)


safeName : Id -> Maybe Participant -> String
safeName id participant =
    case participant of
        Nothing ->
            id |> fallbackName

        Just p ->
            p.name


fallbackName : Id -> String
fallbackName id =
    "<" ++ (id |> idToString) ++ ">"
