module Settlement exposing (..)

import Config exposing (Config)
import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Expense exposing (Expense)
import Domain.Payment exposing (Payment)
import Domain.Suggestion as Suggestion
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Layout
import Maybe.Extra as Maybe
import Participant exposing (lookupName)
import Payment
import Util.Dict as Dict


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
    , suggestedPayments : Dict Int (List Payment.SuggestedPayment)
    }


import_ : List Payment -> Model -> Model
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
        , viewBalanceInstructions
        , viewBalances config participantModel.idToName model
        , Html.h3 [] [ text "Payments" ]
        , viewPaymentsInstructions
        ]
            ++ (Payment.view config participantModel model.payment |> List.map (Html.map PaymentMsg))


viewBalances : Config -> Dict Int String -> Model -> Html Msg
viewBalances config participants model =
    Html.table
        [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "Participant" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Balance" ]
                , Html.th [ Html.Attributes.scope "col" ]
                    [ Html.text "Suggested payments"
                    , let
                        suggestedPayments =
                            model.computed |> Maybe.map .suggestedPayments |> Maybe.withDefault Dict.empty
                      in
                      Html.button
                        [ Html.Attributes.class <|
                            "ms-1 badge btn btn-primary"
                                ++ (if suggestedPayments |> Dict.isEmpty then
                                        " invisible"

                                    else
                                        ""
                                   )
                        , Html.Events.onClick (Payment.ApplySuggestedPayments suggestedPayments |> PaymentMsg)
                        ]
                        [ Html.text "apply all" ]
                    ]
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
                                                        (\( receiverId, suggestedAmount, payment ) ->
                                                            ( participants |> Participant.lookupName receiverId
                                                            , receiverId
                                                            , ( suggestedAmount, payment )
                                                            )
                                                        )
                                                        -- Sort by name, then ID.
                                                        >> List.sortBy (\( receiverName, receiverId, _ ) -> ( receiverName, receiverId ))
                                                        >> List.map
                                                            (\( receiverName, receiverId, ( suggestedAmount, payment ) ) ->
                                                                div []
                                                                    [ Layout.internalLink
                                                                        (Payment.ApplySuggestedPayments
                                                                            (Dict.singleton participantId [ ( receiverId, suggestedAmount, payment ) ])
                                                                            |> PaymentMsg
                                                                        )
                                                                        [ Html.text <|
                                                                            case payment of
                                                                                Nothing ->
                                                                                    "Pay "
                                                                                        ++ Amount.toString config.amount suggestedAmount
                                                                                        ++ " to "
                                                                                        ++ receiverName

                                                                                Just ( paymentId, inverse ) ->
                                                                                    if inverse then
                                                                                        "Receive "
                                                                                            ++ Amount.toString config.amount suggestedAmount
                                                                                            ++ " less from "
                                                                                            ++ receiverName
                                                                                            ++ " in payment #"
                                                                                            ++ String.fromInt paymentId

                                                                                    else
                                                                                        "Pay additional "
                                                                                            ++ Amount.toString config.amount suggestedAmount
                                                                                            ++ " to "
                                                                                            ++ receiverName
                                                                                            ++ " in payment #"
                                                                                            ++ String.fromInt paymentId
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


viewBalanceInstructions : Html msg
viewBalanceInstructions =
    Layout.infoBox
        [ Html.p []
            [ Html.text "The suggested payments provide one possible set of payments that would make all the participants square. Click a suggested payment to \"apply\" it, i.e. add it to the payment component below."
            ]
        ]


viewPaymentsInstructions : Html msg
viewPaymentsInstructions =
    Layout.infoBox
        [ Html.p []
            [ Html.text "Payments are most easily added by applying suggestions from the table above. If, for whatever reason (like cash is involved), a certain payment is particularly convenient, it may be added manually below. The balances and suggestions above will adjust accordingly."
            ]
        , Html.p []
            [ Html.text "Once a payment has actually been done, it may be marked as such to prevent it from being deleted or modified. Deleting planned payments is useful when expenses are added after the balances have been squared."
            ]
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


findExistingPaymentId : Int -> Int -> List Payment -> Maybe ( Int, Bool )
findExistingPaymentId payerId receiverId payments =
    case payments of
        [] ->
            Nothing

        existingPayment :: remainingExistingPayments ->
            if not existingPayment.done && existingPayment.payer == payerId && existingPayment.receiver == receiverId then
                Just ( existingPayment.id, False )

            else if not existingPayment.done && existingPayment.payer == receiverId && existingPayment.receiver == payerId then
                Just ( existingPayment.id, True )

            else
                findExistingPaymentId payerId receiverId remainingExistingPayments


withExistingPaymentId : List Payment -> Int -> ( Int, Amount ) -> Payment.SuggestedPayment
withExistingPaymentId existingPayments payerId ( receiverId, amount ) =
    ( receiverId, amount, findExistingPaymentId payerId receiverId existingPayments )


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
                                    Suggestion.autosuggestPayments (Dict.sumValues balance model.payment.paymentBalance)
                                        |> Dict.map (\payerId -> List.map (withExistingPaymentId model.payment.payments payerId))
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
                                                Suggestion.autosuggestPayments (Dict.sumValues computed.balance paymentModel.paymentBalance)
                                                    |> Dict.map (\payerId -> List.map (withExistingPaymentId paymentModel.payments payerId))
                                        }
                                    )

                        else
                            model.computed
                }
              , modelChanged
              )
            , paymentCmd |> Cmd.map PaymentMsg
            )
