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


suggestPaymentAmount : Participant.Id -> Participant.Id -> Balances -> Balances -> Result ( Maybe Participant.Id, Maybe Participant.Id ) Amount
suggestPaymentAmount payerId receiverId paymentBalance balance =
    let
        payerBalance =
            Balance.sum payerId paymentBalance balance

        receiverBalance =
            Balance.sum receiverId paymentBalance balance

        suggestedAmount =
            min -payerBalance receiverBalance
    in
    if suggestedAmount <= 0 then
        Err
            ( if payerBalance >= 0 then
                Just payerId

              else
                Nothing
            , if receiverBalance <= 0 then
                Just receiverId

              else
                Nothing
            )

    else
        Ok suggestedAmount


findExistingPaymentId : Participant.Id -> Participant.Id -> List Payment -> Maybe ( Payment.Id, Bool )
findExistingPaymentId payerId receiverId payments =
    case payments of
        [] ->
            Nothing

        existingPayment :: remainingExistingPayments ->
            if not existingPayment.done && existingPayment.payer == payerId && existingPayment.receiver == receiverId then
                Just ( existingPayment.id, False )

            else if not existingPayment.done && existingPayment.payer == receiverId && existingPayment.receiver == payerId then
                Just ( existingPayment.id, True )

            else
                findExistingPaymentId payerId receiverId remainingExistingPayments


withExistingPaymentId : List Payment -> Participant.Id -> ( Participant.Id, Amount ) -> SuggestedPayment
withExistingPaymentId existingPayments payerId ( receiverId, amount ) =
    ( receiverId, amount, findExistingPaymentId payerId receiverId existingPayments )
