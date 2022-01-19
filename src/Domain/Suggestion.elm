module Domain.Suggestion exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Balance as Balance exposing (Balances)
import Domain.Payment as Payment exposing (Payment)
import Domain.SettlementGroup as SettlementGroup


{-| Triple of receiver ID, amount, and ID of existing payment to amend.
The payment ID is represented as a pair of the ID and a boolean indicating
whether the amount should be added (false) or subtracted (true).
-}
type alias SuggestedPayment =
    ( SettlementGroup.Id, Amount, Maybe ( Payment.Id, Bool ) )


autosuggestPayments : Balances -> Dict SettlementGroup.Id (List ( SettlementGroup.Id, Amount ))
autosuggestPayments totalBalances =
    case totalBalances |> autosuggestPayment of
        Nothing ->
            Dict.empty

        Just ( payerId, receiverId, amount ) ->
            totalBalances
                |> Balance.transferAmount payerId receiverId amount
                |> autosuggestPayments
                |> Dict.update payerId
                    (Maybe.withDefault []
                        >> (::) ( receiverId, amount )
                        >> Just
                    )


autosuggestPayment : Balances -> Maybe ( SettlementGroup.Id, SettlementGroup.Id, Amount )
autosuggestPayment =
    Balance.findExtremaBalanceGroup
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


suggestPaymentAmount : SettlementGroup.Id -> SettlementGroup.Id -> Balances -> Balances -> Result ( Maybe SettlementGroup.Id, Maybe SettlementGroup.Id ) Amount
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


findExistingPaymentId : SettlementGroup.Id -> SettlementGroup.Id -> List Payment -> Maybe ( Payment.Id, Bool )
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


withExistingPaymentId : List Payment -> SettlementGroup.Id -> ( SettlementGroup.Id, Amount ) -> SuggestedPayment
withExistingPaymentId existingPayments payerId ( receiverId, amount ) =
    ( receiverId, amount, findExistingPaymentId payerId receiverId existingPayments )
