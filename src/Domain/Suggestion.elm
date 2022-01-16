module Domain.Suggestion exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Balance as Balance exposing (Balances)


autosuggestPayments : Balances -> Dict Int (List ( Int, Amount ))
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


autosuggestPayment : Balances -> Maybe ( Int, Int, Amount )
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


suggestPaymentAmount : Int -> Int -> Balances -> Balances -> Result ( Maybe Int, Maybe Int ) Amount
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
