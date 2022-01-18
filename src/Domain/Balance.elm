module Domain.Balance exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)


type alias Balances =
    Dict Int Amount


transferAmount : Int -> Int -> Amount -> Balances -> Balances
transferAmount payerId receiverId amount =
    addAmount payerId amount >> addAmount receiverId -amount


addAmount : Int -> Amount -> Balances -> Balances
addAmount participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)


sum : Int -> Balances -> Balances -> Amount
sum participantId paymentBalance balance =
    (balance |> lookup participantId) + (paymentBalance |> lookup participantId)


lookup : Int -> Balances -> Amount
lookup participantId =
    Dict.get participantId >> Maybe.withDefault 0


findExtremaBalanceParticipants : Balances -> Maybe ( ( Int, Amount ), ( Int, Amount ) )
findExtremaBalanceParticipants =
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
