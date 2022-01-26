module Domain.Settlement exposing (..)

import Dict exposing (Dict)
import Domain.Amount exposing (Amount)
import Domain.Balance as Balance exposing (Balances)
import Domain.Expense as Expense exposing (Debt, Expense, Expenses)
import Domain.Participant as Participant exposing (Participant, Participants)
import Domain.Payment exposing (Payment)
import Domain.Suggestion as Suggestion exposing (SuggestedPayment)
import Util.Dict as Dict


type alias Computed =
    { expenses : Expenses
    , debts : Debt
    , balance : Balances
    , suggestedPayments : Dict Participant.Id (List SuggestedPayment) -- TODO extract type?
    }


compute : Participants -> List Expense -> Dict Participant.Id Amount -> List Payment -> Computed
compute participants expenseList paymentBalance payments =
    let
        expenses =
            Expense.expensesFromList expenseList

        debts =
            Expense.invert expenses

        balances =
            participants
                |> Dict.values
                |> List.map .id
                |> List.foldl
                    (\participantId ->
                        (Dict.valueSum participantId expenses - Dict.valueSum participantId debts)
                            |> Dict.insert participantId
                    )
                    Dict.empty

        suggestedPayments =
            -- The result value of sumValues only contains keys from the first argument.
            Dict.sumValues balances paymentBalance
                |> Suggestion.autosuggestPayments
                |> Dict.map (\payerId -> List.map (Suggestion.withExistingPaymentId payments payerId))
    in
    { expenses = expenses
    , debts = debts
    , balance = balances
    , suggestedPayments = suggestedPayments
    }
