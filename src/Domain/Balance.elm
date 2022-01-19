module Domain.Balance exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.SettlementGroup as SettlementGroup


type alias Balances =
    Dict SettlementGroup.Id Amount


transferAmount : SettlementGroup.Id -> SettlementGroup.Id -> Amount -> Balances -> Balances
transferAmount payerId receiverId amount =
    addAmount payerId amount >> addAmount receiverId -amount


addAmount : SettlementGroup.Id -> Amount -> Balances -> Balances
addAmount participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)


sum : SettlementGroup.Id -> Balances -> Balances -> Amount
sum participantId paymentBalance balance =
    (balance |> lookup participantId) + (paymentBalance |> lookup participantId)


lookup : SettlementGroup.Id -> Balances -> Amount
lookup participantId =
    Dict.get participantId >> Maybe.withDefault 0


findExtremaBalanceGroup : Balances -> Maybe ( ( SettlementGroup.Id, Amount ), ( SettlementGroup.Id, Amount ) )
findExtremaBalanceGroup =
    Dict.foldl
        (\participantId participantBalance result ->
            case result of
                Nothing ->
                    Just ( ( participantId, participantBalance ), ( participantId, participantBalance ) )

                Just ( ( _, minAmount ) as minResult, ( _, maxAmount ) as maxResult ) ->
                    if participantBalance < minAmount then
                        Just ( ( participantId, participantBalance ), maxResult )

                    else if participantBalance > maxAmount then
                        Just ( minResult, ( participantId, participantBalance ) )

                    else
                        result
        )
        Nothing
