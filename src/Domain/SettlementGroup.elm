module Domain.SettlementGroup exposing (..)

import Dict exposing (Dict)
import Domain.Participant as Participant


type alias Id =
    Int


idFromString : String -> Maybe Id
idFromString =
    String.toInt


idToString : Id -> String
idToString =
    String.fromInt


type alias SettlementGroup =
    { id : Id
    , name : String
    , participants : List Participant.Id
    }


type alias SettlementGroups =
    List SettlementGroup


type alias NameIndex =
    Dict Id String


lookupName : Id -> NameIndex -> String
lookupName id index =
    case Dict.get id index of
        Nothing ->
            "<" ++ idToString id ++ ">"

        Just name ->
            name
