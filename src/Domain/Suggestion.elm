module Domain.Suggestion exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Balance as Balance exposing (Balances)
import Domain.Participant as Participant
import Domain.Payment as Payment exposing (Payment)


{-| Triple of receiver ID, amount, and ID of existing payment to amend.
The payment ID is represented as a pair of the ID and a boolean indicating
whether the amount should be added (false) or subtracted (true).
-}
type alias SuggestedPayment =
    ( Participant.Id, Amount, Maybe ( Payment.Id, Bool ) )


autosuggestPayments : Balances -> Dict Participant.Id (List ( Participant.Id, Amount ))
autosuggestPayments totalBalances =
    case totalBalances |> autosuggestPayment of
        Nothing ->
            Dict.empty

        Just ( payerId, receiverId, amount ) ->
            totalBalances
                |> Balance.transfer payerId receiverId amount
                |> autosuggestPayments
                |> Dict.update payerId
                    (Maybe.withDefault []
                        >> (::) ( receiverId, amount )
                        >> Just
                    )


autosuggestPayment : Balances -> Maybe ( Participant.Id, Participant.Id, Amount )
autosuggestPayment =
    Balance.findExtremaBalanceParticipants
        >> Maybe.andThen
            (\( ( minParticipant, minBalance ), ( maxParticipant, maxBalance ) ) ->
                let
                    debt =
                        min maxBalance -minBalance
                in
                if debt == 0 then
                    Nothing

                else
                    Just ( minParticipant, maxParticipant, debt )
            )


findExistingPaymentId : Participant.Id -> Participant.Id -> List Payment -> Maybe ( Payment.Id, Bool )
findExistingPaymentId payerId receiverId =
    List.filter (not << .done)
        >> findExistingPayments payerId receiverId
        >> List.head
        >> Maybe.map (Tuple.mapFirst .id)


findExistingPayments : Participant.Id -> Participant.Id -> List Payment -> List ( Payment, Bool )
findExistingPayments payerId receiverId =
    List.filterMap
        (\payment ->
            if payment.payer == payerId && payment.receiver == receiverId then
                Just ( payment, False )

            else if payment.payer == receiverId && payment.receiver == payerId then
                Just ( payment, True )

            else
                Nothing
        )


withExistingPaymentId : List Payment -> Participant.Id -> ( Participant.Id, Amount ) -> SuggestedPayment
withExistingPaymentId existingPayments payerId ( receiverId, amount ) =
    ( receiverId, amount, findExistingPaymentId payerId receiverId existingPayments )
