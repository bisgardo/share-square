module Domain.Participant exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode


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
        -- settled by
        (Decode.maybe <| Decode.field "s" Decode.int)


encode : Participant -> Value
encode participant =
    [ ( "i", participant.id |> Encode.int )
    , ( "n", participant.name |> Encode.string )
    , ( "s", participant.settledBy |> Encode.maybe Encode.int )
    ]
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



--resolveSettledBy : Participants -> Set Id -> Participant -> Maybe Id
--resolveSettledBy participants seen participant =
--    participant.settledBy
--        |> Maybe.andThen
--            (\settledById ->
--                if seen |> Set.member settledById then
--                    Nothing
--
--                else
--                    participants
--                        |> Dict.get settledById
--                        |> Maybe.andThen
--                            (\settledBy ->
--                                resolveSettledBy participants (seen |> Set.insert settledById) settledBy
--                            )
--                        |> Maybe.orElse (Just settledById)
--            )
