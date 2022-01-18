module Settlement exposing (..)

import Config exposing (Config)
import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Balance exposing (Balances)
import Domain.Expense as Expense exposing (Debt, Expense, Expenses)
import Domain.Participant as Participant
import Domain.Payment as Payment exposing (Payment)
import Domain.Suggestion as Suggestion exposing (SuggestedPayment)
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
    , debts : Debt
    , balance : Balances
    , suggestedPayments : Dict Participant.Id (List SuggestedPayment) -- TODO extract type?
    }


import_ : List Payment -> Model -> Model
import_ payments model =
    { model
        | computed = Nothing
        , payment = model.payment |> Payment.import_ payments
    }


type Msg
    = Disable
    | Enable (List Participant.Id) (List Expense)
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


viewBalances : Config -> Dict Participant.Id String -> Model -> Html Msg
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
                                    ( participantId |> Participant.idToString
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
                                                                                            ++ Payment.idToString paymentId

                                                                                    else
                                                                                        "Pay additional "
                                                                                            ++ Amount.toString config.amount suggestedAmount
                                                                                            ++ " to "
                                                                                            ++ receiverName
                                                                                            ++ " in payment #"
                                                                                            ++ Payment.idToString paymentId
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
                        Expense.expensesFromList expenseList

                    debts =
                        Expense.invert expenses

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
                                        |> Dict.map (\payerId -> List.map (Suggestion.withExistingPaymentId model.payment.payments payerId))
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
                                                    |> Dict.map (\payerId -> List.map (Suggestion.withExistingPaymentId paymentModel.payments payerId))
                                        }
                                    )

                        else
                            model.computed
                }
              , modelChanged
              )
            , paymentCmd |> Cmd.map PaymentMsg
            )
