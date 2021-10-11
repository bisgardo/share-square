module Computation exposing (..)

import Dict exposing (Dict)
import Expenses exposing (Expense)
import Html exposing (Html, div, text)
import Layout exposing (..)
import Members exposing (Member)
import Round
import Util.Dict as Dict


view : List Member -> Expenses -> Html msg
view members expenses =
    row
        [ div [] <|
            [ Html.h2 [] [ text "Outlays" ] ]
                ++ (expenses
                        |> Dict.toFlatList
                        |> List.map
                            (\( payer, receiver, amount ) ->
                                div []
                                    [ text <|
                                        Members.nameFromId payer members
                                            ++ " has expended "
                                            ++ (amount |> Round.round 2)
                                            ++ " for "
                                            ++ Members.nameFromId receiver members
                                            ++ "."
                                    ]
                            )
                   )
        , div [] <|
            [ Html.h2 [] [ text "Debt/receivables" ] ]
                ++ (expenses
                        |> invert
                        |> Dict.toFlatList
                        |> List.map
                            (\( receiver, payer, amount ) ->
                                div []
                                    [ text <|
                                        Members.nameFromId receiver members
                                            ++ " owes "
                                            ++ Members.nameFromId payer members
                                            ++ " "
                                            ++ (amount |> Round.round 2)
                                            ++ "."
                                    ]
                            )
                   )
        ]


{-| A dict from ID of payer to dict from ID of receiver to totally expensed amount.
-}
type alias Expenses =
    Dict Int (Dict Int Float)


{-| A dict from ID of receiver to dict from ID of payer to totally expensed amount.
-}
type alias Debt =
    Expenses


expensesFromList : List Expense -> Expenses
expensesFromList =
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
                    >> Dict.sumValues weightedDebt
                    >> Just
                )
        )
        Dict.empty


invert : Expenses -> Debt
invert =
    Dict.foldl
        (\payer payerExpenses result ->
            Dict.foldl
                (\receiver amount ->
                    Dict.update receiver
                        (Maybe.withDefault Dict.empty
                            >> Dict.update payer
                                (Maybe.withDefault 0
                                    >> (+) amount
                                    >> Just
                                )
                            >> Just
                        )
                )
                result
                payerExpenses
        )
        Dict.empty
