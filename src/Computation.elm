module Computation exposing (..)

import Dict exposing (Dict)
import Expense exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Layout exposing (..)
import Participant exposing (lookupName)
import Round
import Util.Dict as Dict


type alias Model =
    { summaryPerspective : SummaryPerspective
    , computed : Maybe ComputedModel
    }


init : Model
init =
    { summaryPerspective = SummaryPerspectiveOutlays
    , computed = Nothing
    }


type alias ComputedModel =
    { expenses : Expenses
    , debts : Expenses
    , balance : Dict Int Float
    }


type SummaryPerspective
    = SummaryPerspectiveOutlays
    | SummaryPerspectiveDebt


type Msg
    = SetSummaryPerspective SummaryPerspective
    | Recompute (List Int) (List Expense)


view : Dict Int String -> Model -> Html Msg
view participants model =
    row
        [ viewSummary participants model
        , viewBalance participants model
        ]


viewSummary : Dict Int String -> Model -> Html Msg
viewSummary participants model =
    div []
        [ Html.h3 [] [ text "Summary" ]
        , Html.p []
            [ div [ Html.Attributes.class "form-check form-check-inline" ]
                [ Html.input
                    [ Html.Attributes.class "form-check-input"
                    , Html.Attributes.id "computation-summary-outlay"
                    , Html.Attributes.name "computation-summary"
                    , Html.Attributes.type_ "radio"
                    , model.summaryPerspective == SummaryPerspectiveOutlays |> Html.Attributes.checked
                    , Html.Events.onCheck (SetSummaryPerspective SummaryPerspectiveOutlays |> always)
                    ]
                    []
                , Html.label
                    [ Html.Attributes.class "form-check-label"
                    , Html.Attributes.for "computation-summary-outlay"
                    ]
                    [ text "Outlays" ]
                ]
            , div [ Html.Attributes.class "form-check form-check-inline" ]
                [ Html.input
                    [ Html.Attributes.class "form-check-input"
                    , Html.Attributes.id "computation-summary-debt"
                    , Html.Attributes.name "computation-summary"
                    , Html.Attributes.type_ "radio"
                    , model.summaryPerspective == SummaryPerspectiveDebt |> Html.Attributes.checked
                    , Html.Events.onCheck (SetSummaryPerspective SummaryPerspectiveDebt |> always)
                    ]
                    []
                , Html.label
                    [ Html.Attributes.class "form-check-label"
                    , Html.Attributes.for "computation-summary-debt"
                    ]
                    [ text "Debt" ]
                ]
            ]
        , case model.computed of
            Nothing ->
                Html.p [] [ Html.em [] [ text "No result available yet." ] ]

            Just computed ->
                Html.p []
                    [ Html.ul [] <|
                        case model.summaryPerspective of
                            SummaryPerspectiveOutlays ->
                                computed.expenses
                                    |> Dict.toFlatList
                                    |> List.map
                                        (\( payer, receiver, amount ) ->
                                            ( lookupName payer participants
                                            , lookupName receiver participants
                                            , amount |> Round.round 2
                                            )
                                        )
                                    |> List.sort
                                    |> List.map
                                        (\( payer, receiver, amount ) ->
                                            Html.li []
                                                [ payer ++ " has expended " ++ amount ++ " for " ++ receiver ++ "." |> text ]
                                        )

                            SummaryPerspectiveDebt ->
                                computed.debts
                                    |> Dict.toFlatList
                                    |> List.map
                                        (\( receiver, payer, amount ) ->
                                            ( lookupName receiver participants
                                            , lookupName payer participants
                                            , amount |> Round.round 2
                                            )
                                        )
                                    |> List.sort
                                    |> List.map
                                        (\( receiver, payer, amount ) ->
                                            Html.li []
                                                [ receiver ++ " owes " ++ payer ++ " " ++ amount ++ "." |> text ]
                                        )
                    ]
        ]


viewBalance : Dict Int String -> Model -> Html Msg
viewBalance participants model =
    div [] <|
        [ Html.h3 [] [ text "Balances" ]
        , Html.p []
            [ Html.ul []
                (case model.computed of
                    Nothing ->
                        []

                    Just computed ->
                        computed.balance
                            |> Dict.toList
                            |> List.map
                                (\( participantId, amount ) ->
                                    ( lookupName participantId participants
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
                                (\( participant, amount ) ->
                                    Html.li [] [ participant ++ ": " ++ amount |> text ]
                                )
                )
            ]
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
        SetSummaryPerspective value ->
            ( { model | summaryPerspective = value }, Cmd.none )

        Recompute participantIds expenseList ->
            let
                expenses =
                    expensesFromList expenseList

                debts =
                    invert expenses

                balance =
                    participantIds
                        |> List.foldl
                            (\participant ->
                                Dict.insert participant (Dict.valueSum participant expenses - Dict.valueSum participant debts)
                            )
                            Dict.empty
            in
            ( { model
                | computed =
                    Just
                        { expenses = expenses
                        , debts = debts
                        , balance = balance
                        }
              }
            , Cmd.none
            )
