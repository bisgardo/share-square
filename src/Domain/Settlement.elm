module Domain.Settlement exposing (..)

import Dict exposing (Dict)
import Domain.Amount exposing (Amount)
import Domain.Balance as Balance exposing (Balances)
import Domain.Expense as Expense exposing (Debt, Expense, Expenses)
import Domain.Participant as Participant exposing (Participant, Participants)
import Domain.Payment exposing (Payment)
import Domain.Suggestion as Suggestion exposing (SuggestedPayment)
import Maybe.Extra as Maybe
import Util.Dict as Dict


type alias Computed =
    { expenses : Expenses
    , debts : Debt
    , balance : Balances
    , suggestedPayments : Dict Participant.Id (List SuggestedPayment) -- TODO extract type?
    }


compute : List Participant -> List Expense -> Dict Participant.Id Amount -> List Payment -> Computed
compute participants expenseList paymentBalance payments =
    let
        expenses =
            Expense.expensesFromList expenseList

        debts =
            Expense.invert expenses

        balances =
            participants
                |> List.map .id
                |> List.foldl
                    (\participantId ->
                        (Dict.valueSum participantId expenses - Dict.valueSum participantId debts)
                            |> Dict.insert participantId
                    )
                    Dict.empty

        suggestedPayments =
            let
                _ =
                    Debug.log "running from compute" 1
            in
            -- The result value of sumValues only contains keys from the first argument.
            Dict.sumValues balances paymentBalance
                |> applySettledBy participants payments
                |> Suggestion.autosuggestPayments
                |> Dict.map (\payerId -> List.map (Suggestion.withExistingPaymentId payments payerId))
    in
    { expenses = expenses
    , debts = debts
    , balance = balances
    , suggestedPayments = suggestedPayments
    }


applySettledBy : List Participant -> List Payment -> Balances -> Balances
applySettledBy participants payments balances =
    let
        -- Might want to represent settlement relation this way in the first place...
        settledBy =
            participants
                |> List.foldl
                    (\participant ->
                        case participant.settledBy of
                            Nothing ->
                                identity

                            Just settledById ->
                                Dict.insert participant.id settledById
                    )
                    Dict.empty

        -- Balances adjusted for payments between participants that settle for each other.
        settlementBalances =
            payments
                |> List.foldl
                    (\payment ->
                        let
                            payerSettledByReceiver =
                                settledBy
                                    |> Dict.get payment.payer
                                    |> Maybe.unwrap False (\payerSettledById -> payment.receiver == payerSettledById)

                            receiverSettledByPayer =
                                settledBy
                                    |> Dict.get payment.receiver
                                    |> Maybe.unwrap False (\receiverSettledById -> payment.payer == receiverSettledById)
                        in
                        if payerSettledByReceiver || receiverSettledByPayer then
                            Balance.transfer payment.receiver payment.payer payment.amount

                        else
                            identity
                    )
                    balances
    in
    participants
        |> List.foldl
            (\participant ->
                case participant.settledBy of
                    Nothing ->
                        identity

                    Just settledById ->
                        settlementBalances
                            |> Dict.get participant.id
                            |> Maybe.withDefault 0
                            |> Balance.transfer
                                settledById
                                participant.id
            )
            balances
