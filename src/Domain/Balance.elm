module Domain.Balance exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Participant as Participant


{-| A dictionary of balances for all participants.
The balance of a participant is defined as the amount that the other participants collectively owe that participant.
This is conceptually different from ordinary bank account balances which designates how much someone is allowed to spend.
as spending money makes your balance to up (and vice versa).
-}
type alias Balances =
    Dict Participant.Id Amount


{-| Transfer a given amount from the balance of one participant to another.
Note that payment transfers increase the balance of the payer and reduces it for the receiver.
-}
transfer : Participant.Id -> Participant.Id -> Amount -> Balances -> Balances
transfer payerId receiverId amount =
    add payerId amount >> add receiverId -amount


{-| Increase the balance of a given participant by a given amount.
-}
add : Participant.Id -> Amount -> Balances -> Balances
add participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)


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
