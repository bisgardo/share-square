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


type alias Index =
    Dict Id SettlementGroup


lookup : Id -> Index -> Maybe SettlementGroup
lookup =
    Dict.get


lookupName : Id -> Index -> String
lookupName id index =
    case lookup id index of
        Nothing ->
            "<" ++ idToString id ++ ">"

        Just settlementGroup ->
            settlementGroup.name
