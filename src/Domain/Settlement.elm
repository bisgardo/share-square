module Domain.Settlement exposing (..)

import Dict exposing (Dict)
import Domain.Balance as Balance exposing (Balances)
import Domain.Expense as Expense exposing (Debt, Expense, Expenses)
import Domain.Participant as Participant exposing (Participant, Participants)
import Domain.Payment exposing (Payment)
import Domain.Suggestion as Suggestion exposing (SuggestedPayments)
import Maybe.Extra as Maybe
import Util.Dict as Dict


type alias SettledBy =
    Dict Participant.Id Participant.Id


type alias Computed =
    { expenses : Expenses
    , debts : Debt
    , balance : Balances
    , suggestedPayments : SuggestedPayments
    }


compute : List Participant -> List Expense -> Balances -> List Payment -> SettledBy -> Computed
compute participants expenseList paymentBalance payments settledBy =
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
    in
    { expenses = expenses
    , debts = debts
    , balance = balances
    , suggestedPayments = computeSuggestedPayments balances paymentBalance participants payments settledBy
    }


computeSuggestedPayments : Balances -> Balances -> List Participant -> List Payment -> SettledBy -> SuggestedPayments
computeSuggestedPayments balances paymentBalance participants payments settledBy =
    balances
        |> Dict.sumValues paymentBalance
        |> applySettledBy participants settledBy payments
        |> Suggestion.autosuggestPayments
        |> Dict.map
            (\payerId ->
                List.map
                    (Suggestion.withExistingPaymentId payments payerId)
            )


applySettledBy : List Participant -> SettledBy -> List Payment -> Balances -> Balances
applySettledBy participants settledBy payments balances =
    let
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
                case settledBy |> Dict.get participant.id of
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
