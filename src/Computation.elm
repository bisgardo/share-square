module Computation exposing (..)

import Amount exposing (Amount)
import Dict exposing (Dict)
import Expense exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Layout
import Config exposing (Config)
import Maybe.Extra as Maybe
import Participant exposing (lookupName)
import Payment
import Util.Dict as Dict
import Util.Maybe as Maybe


type alias Model =
    { computed : Maybe ComputedModel
    , payment : Payment.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( paymentModel, paymentCmd ) =
            Payment.init
    in
    ( { computed = Nothing
      , payment = paymentModel
      }
    , paymentCmd |> Cmd.map PaymentMsg
    )


type alias ComputedModel =
    { expenses : Expenses
    , debts : Expenses
    , balance : Dict Int Amount
    , suggestedPayments : Dict Int (List ( Int, Amount ))
    }


import_ : Payment.StorageValues -> Model -> Model
import_ payments model =
    { model
        | computed = Nothing
        , payment = model.payment |> Payment.import_ payments
    }


type Msg
    = Disable
    | Enable (List Int) (List Expense)
    | PaymentMsg Payment.Msg


view : Config -> Participant.Model -> Model -> Html Msg
view config participantModel model =
    div [ Html.Attributes.class "col" ] <|
        [ Html.h3 [] [ text "Balances" ]
        , viewBalances config participantModel.idToName model
        , Html.h3 [] [ text "Payments" ]
        ]
            ++ (Payment.view config participantModel model.payment |> List.map (Html.map PaymentMsg))


viewBalances : Config -> Dict Int String -> Model -> Html Msg
viewBalances config participants model =
    Html.table [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "Participant" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Balance" ]
                , Html.th [ Html.Attributes.scope "col" ] <|
                    [ Html.text "Suggested payments" ]
                        ++ (case model.computed |> Maybe.map .suggestedPayments |> Maybe.nothingIf Dict.isEmpty of
                                Nothing ->
                                    []

                                Just suggestedPayments ->
                                    [ Html.button
                                        [ Html.Attributes.class "ms-1 badge btn btn-primary"
                                        , Html.Events.onClick (Payment.ApplyAllSuggestedPayments suggestedPayments |> PaymentMsg)
                                        ]
                                        [ Html.text "apply all" ]
                                    ]
                           )
                ]
            ]
        , Html.Keyed.node "tbody"
            []
            (model.computed
                |> Maybe.unwrap []
                    (\computed ->
                        computed.balance
                            |> Dict.toList
                            |> List.map
                                (\( participantId, expendedAmount ) ->
                                    let
                                        participantName =
                                            lookupName participantId participants

                                        paymentBalance =
                                            model.payment.paymentBalance
                                                |> Dict.get participantId
                                                |> Maybe.withDefault 0

                                        totalBalance =
                                            expendedAmount + paymentBalance
                                    in
                                    ( participantName
                                    , participantId
                                    , totalBalance
                                    )
                                )
                            -- Sort by name, then ID.
                            >> List.sort
                            >> List.map
                                (\( participantName, participantId, amount ) ->
                                    ( participantId |> String.fromInt
                                    , Html.tr
                                        [ Html.Attributes.class <|
                                            if amount < 0 then
                                                "text-danger"

                                            else if amount > 0 then
                                                "text-success"

                                            else
                                                "text-decoration-line-through"
                                        ]
                                        [ Html.td [] [ text participantName ]
                                        , Html.td [] [ text (amount |> Amount.toStringSigned "+" config.amount) ]
                                        , Html.td []
                                            (computed.suggestedPayments
                                                |> Dict.get participantId
                                                |> Maybe.unwrap []
                                                    (List.map
                                                        (\( receiverId, suggestedAmount ) ->
                                                            ( participants |> Participant.lookupName receiverId
                                                            , receiverId
                                                            , suggestedAmount
                                                            )
                                                        )
                                                        -- Sort by name, then ID.
                                                        >> List.sort
                                                        >> List.map
                                                            (\( receiverName, receiverId, suggestedAmount ) ->
                                                                div []
                                                                    [ Layout.internalLink
                                                                        (Payment.ApplySuggestedPayment
                                                                            participantId
                                                                            receiverId
                                                                            suggestedAmount
                                                                            |> PaymentMsg
                                                                        )
                                                                        [ Html.text <|
                                                                            "Pay "
                                                                                ++ Amount.toString config.amount suggestedAmount
                                                                                ++ " to "
                                                                                ++ receiverName
                                                                        ]
                                                                    ]
                                                            )
                                                    )
                                            )
                                        ]
                                    )
                                )
                    )
            )
        ]


{-| A dict from ID of payer to dict from ID of receiver to totally expensed amount.
-}
type alias Expenses =
    Dict Int (Dict Int Amount)


{-| A dict from ID of receiver to dict from ID of payer to totally expensed amount.
-}
type alias Debt =
    Expenses


expensesFromList : List Expense -> Expenses
expensesFromList =
    List.foldl
        (\expense outerResult ->
            let
                weightSum =
                    expense.receivers
                        |> Dict.values
                        |> List.sum

                weightedDebt =
                    expense.receivers
                        |> Dict.foldl
                            (\receiver part innerResult ->
                                if receiver == expense.payer then
                                    -- Ignore debt to self.
                                    innerResult

                                else
                                    innerResult
                                        |> Dict.insert receiver (part * (expense.amount |> toFloat) / weightSum |> round)
                            )
                            Dict.empty
            in
            if weightedDebt |> Dict.isEmpty then
                -- Ignore entry if the payer is the only receiver of the expense.
                outerResult

            else
                Dict.update expense.payer
                    (Maybe.withDefault Dict.empty
                        >> Dict.sumValues weightedDebt
                        >> Just
                    )
                    outerResult
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
                            >> Dict.update payer (Maybe.withDefault 0 >> (+) amount >> Just)
                            >> Just
                        )
                )
                result
                payerExpenses
        )
        Dict.empty


subscriptions : Model -> Sub Msg
subscriptions =
    .payment >> Payment.subscriptions >> Sub.map PaymentMsg


update : Config -> Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update config msg model =
    case msg of
        Disable ->
            ( ( { model | computed = Nothing }, False ), Cmd.none )

        Enable participantIds expenseList ->
            -- TODO Instead of enable/disable, detect if the expense list actually changed and only recompute if it did.
            --      If only the participant list changed, just add/remove the relevant balance entries.
            if Maybe.isJust model.computed then
                ( ( model, False ), Cmd.none )

            else
                let
                    expenses =
                        expensesFromList expenseList

                    debts =
                        invert expenses

                    balance =
                        participantIds
                            |> List.foldl
                                (\participant ->
                                    (Dict.valueSum participant expenses - Dict.valueSum participant debts)
                                        |> Dict.insert participant
                                )
                                Dict.empty
                in
                ( ( { model
                        | computed =
                            Just
                                { expenses = expenses
                                , debts = debts
                                , balance = balance
                                , suggestedPayments =
                                    -- The result value of sumValues only contains keys from the first argument.
                                    Payment.autosuggestPayments (Dict.sumValues balance model.payment.paymentBalance)
                                }
                    }
                  , False
                  )
                , Cmd.none
                )

        PaymentMsg paymentMsg ->
            let
                ( ( paymentModel, modelChanged ), paymentCmd ) =
                    model.payment |> Payment.update config (model.computed |> Maybe.map .balance) paymentMsg
            in
            ( ( { model
                    | payment = paymentModel
                    , computed =
                        if modelChanged then
                            model.computed
                                |> Maybe.map
                                    (\computed ->
                                        { computed
                                            | suggestedPayments =
                                                Payment.autosuggestPayments (Dict.sumValues computed.balance paymentModel.paymentBalance)
                                        }
                                    )

                        else
                            model.computed
                }
              , modelChanged
              )
            , paymentCmd |> Cmd.map PaymentMsg
            )
