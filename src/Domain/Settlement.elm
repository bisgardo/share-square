module Domain.Settlement exposing (..)

import Dict exposing (Dict)
import Domain.Balance as Balance exposing (Balances)
import Domain.Expense as Expense exposing (Debt, Expense, Expenses)
import Domain.Participant as Participant exposing (Participant, Participants)
import Domain.Payment exposing (Payment)
import Domain.Suggestion as Suggestion exposing (SuggestedPayments)
import Util.Dict as Dict


type alias Computed =
    { expenses : Expenses
    , debts : Debt
    , balance : Balances
    , suggestedPayments : SuggestedPayments
    }


compute : List Participant -> List Expense -> Balances -> List Payment -> Computed
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
            balances
                |> Dict.sumValues paymentBalance
                |> Suggestion.autosuggestPayments
                |> Dict.map
                    (\payerId ->
                        List.map
                            (Suggestion.withExistingPaymentId payments payerId)
                    )
    in
    { expenses = expenses
    , debts = debts
    , balance = balances
    , suggestedPayments = suggestedPayments
    }
