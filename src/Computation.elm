module Computation exposing (..)

import Dict exposing (Dict)
import Expenses exposing (Expense)
import Html exposing (Html, div, text)
import Layout exposing (..)
import Members exposing (Member)
import Round
import Util


view : List Member -> List Expense -> Html msg
view members expenses =
    row <|
        -- TODO Reduce (and invert?) the debt thing
        (debtsFromExpenses expenses
            |> flattenBiKeyDict
            |> List.map
                (\( payer, receiver, amount ) ->
                    div []
                        [ text <|
                            "Payer: "
                                ++ Members.nameFromId payer members
                                ++ ", receiver: "
                                ++ Members.nameFromId receiver members
                                ++ ", amount: "
                                ++ (amount |> Round.round 2)
                        ]
                )
        )


flattenBiKeyDict : Dict a (Dict b c) -> List ( a, b, c )
flattenBiKeyDict =
    Dict.toList
        >> List.map
            (\( outerKey, innerDict ) ->
                innerDict
                    |> Dict.toList
                    |> List.map
                        (\( innerKey, value ) ->
                            ( outerKey, innerKey, value )
                        )
            )
        >> List.concat


type alias Debt =
    Dict Int (Dict Int Float)


debtsFromExpenses : List Expense -> Debt
debtsFromExpenses =
    List.foldl
        (\expense ->
            let
                weightSum =
                    expense.receivers |> Dict.values |> List.sum

                weightedDebt =
                    expense.receivers
                        |> Dict.foldl
                            (\receiver amount result ->
                                if receiver == expense.payer then
                                    -- Ignore debt to self.
                                    result

                                else
                                    Dict.insert receiver (amount * expense.amount / weightSum) result
                            )
                            Dict.empty
            in
            Dict.update expense.payer
                (Maybe.withDefault Dict.empty
                    >> Util.sumDictValues weightedDebt
                    >> Just
                )
        )
        Dict.empty
