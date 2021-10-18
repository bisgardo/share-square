module Computation exposing (..)

import Dict exposing (Dict)
import Expenses exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Layout exposing (..)
import Members exposing (Member)
import Round
import Util.Dict as Dict


type alias Model =
    { viewInverted : Bool
    , computed : Maybe ComputedModel
    }


init : Model
init =
    { viewInverted = False
    , computed = Nothing
    }


type alias ComputedModel =
    { members : Dict Int String
    , expenses : Expenses
    , debts : Expenses
    , balance : Dict Int Float
    }


type Msg
    = SetInvert Bool
    | Recompute (List Member) (List Expense)


view : Dict Int String -> Model -> Html Msg
view members model =
    row
        [ viewSummary members model
        , viewBalance members model
        ]


viewSummary : Dict Int String -> Model -> Html Msg
viewSummary members model =
    div []
        [ Html.h1 [] [ text "Summary" ]
        , div [ Html.Attributes.class "form-check" ]
            [ Html.input [ Html.Attributes.class "form-check-input", Html.Attributes.id "computation-invert", Html.Attributes.type_ "checkbox", Html.Attributes.checked model.viewInverted, Html.Events.onCheck SetInvert ] []
            , Html.label [ Html.Attributes.class "form-check-label", Html.Attributes.for "computation-invert" ] [ text "Invert" ]
            ]
        , case model.computed of
            Nothing ->
                div [] [ Html.em [] [ text "No result available yet." ] ]

            Just computed ->
                div [] <|
                    if model.viewInverted then
                        computed.debts
                            |> Dict.toFlatList
                            |> List.map
                                (\( receiver, payer, amount ) ->
                                    ( Members.lookupName receiver members
                                    , Members.lookupName payer members
                                    , amount |> Round.round 2
                                    )
                                )
                            |> List.sort
                            |> List.map
                                (\( receiver, payer, amount ) ->
                                    div []
                                        [ receiver ++ " owes " ++ payer ++ " " ++ amount ++ "." |> text ]
                                )

                    else
                        computed.expenses
                            |> Dict.toFlatList
                            |> List.map
                                (\( payer, receiver, amount ) ->
                                    ( Members.lookupName payer computed.members
                                    , Members.lookupName receiver computed.members
                                    , amount |> Round.round 2
                                    )
                                )
                            |> List.sort
                            |> List.map
                                (\( payer, receiver, amount ) ->
                                    div []
                                        [ payer ++ " has expended " ++ amount ++ " for " ++ receiver ++ "." |> text ]
                                )
        ]


viewBalance : Dict Int String -> Model -> Html Msg
viewBalance members model =
    div [] <|
        [ Html.h1 [] [ text "Balances" ]
        ]
            ++ (case model.computed of
                    Nothing ->
                        []

                    Just computed ->
                        computed.balance
                            |> Dict.toList
                            |> List.map
                                (\( memberId, amount ) ->
                                    ( Members.lookupName memberId members
                                    , amount
                                        |> Round.round 2
                                        |> (\string ->
                                                if String.startsWith "-" string then
                                                    string

                                                else
                                                    "+" ++ string
                                           )
                                    )
                                )
                            |> List.sort
                            |> List.map
                                (\( member, amount ) ->
                                    Html.div [] [ member ++ ": " ++ amount |> text ]
                                )
               )


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
                    expense.receivers
                        |> Dict.values
                        |> List.sum

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


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        SetInvert value ->
            ( { model | viewInverted = value }, Cmd.none )

        Recompute memberList expenseList ->
            let
                expenses =
                    expensesFromList expenseList

                debts =
                    invert expenses

                balance =
                    memberList
                        |> List.map .id
                        |> List.foldl
                            (\member ->
                                Dict.insert member (Dict.valueSum member expenses - Dict.valueSum member debts)
                            )
                            Dict.empty

                members =
                    List.foldl (\member -> Dict.insert member.id member.name) Dict.empty memberList
            in
            ( { model
                | computed =
                    Just
                        { expenses = expenses
                        , debts = debts
                        , balance = balance
                        , members = members
                        }
              }
            , Cmd.none
            )
