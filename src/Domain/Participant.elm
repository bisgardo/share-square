module Domain.Participant exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe


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
    , settledBy : Maybe Id
    }


decoder : Decoder Participant
decoder =
    Decode.map3
        Participant
        -- ID
        (Decode.field "i" Decode.int)
        -- name
        (Decode.field "n" Decode.string)
        -- settled by (silently ignoring any decoding errors)
        (Decode.field "s" Decode.int |> Decode.maybe)


encode : Participant -> Value
encode participant =
    [ Just ( "i", participant.id |> Encode.int )
    , Just ( "n", participant.name |> Encode.string )
    , participant.settledBy
        |> Maybe.map
            (\settledBy ->
                ( "s", settledBy |> Encode.int )
            )
    ]
        |> Maybe.values
        |> Encode.object


type alias Participants =
    Dict Id Participant


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
