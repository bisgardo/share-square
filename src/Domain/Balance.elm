module Domain.Balance exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Participant as Participant


type alias Balances =
    Dict Participant.Id Amount



-- TODO Document functions and that payment implies a reverse balance transfer.


transfer : Participant.Id -> Participant.Id -> Amount -> Balances -> Balances
transfer payerId receiverId amount =
    add payerId amount >> add receiverId -amount


add : Participant.Id -> Amount -> Balances -> Balances
add participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)


sum : Participant.Id -> Balances -> Balances -> Amount
sum participantId paymentBalance balance =
    (balance |> lookup participantId) + (paymentBalance |> lookup participantId)


lookup : Participant.Id -> Balances -> Amount
lookup participantId =
    Dict.get participantId >> Maybe.withDefault 0


findExtremaBalanceParticipants : Balances -> Maybe ( ( Participant.Id, Amount ), ( Participant.Id, Amount ) )
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
