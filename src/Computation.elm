module Computation exposing (..)

import Dict exposing (Dict)
import Expense exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode
import Layout exposing (..)
import Maybe.Extra as Maybe
import Participant exposing (lookupName)
import Payment
import Util.Dict as Dict
import Util.String as String


type alias Model =
    { summaryPerspective : SummaryPerspective
    , computed : Maybe ComputedModel
    , payment : Payment.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( paymentModel, paymentCmd ) =
            Payment.init
    in
    ( { summaryPerspective = SummaryPerspectiveOutlays
      , computed = Nothing
      , payment = paymentModel
      }
    , paymentCmd |> Cmd.map PaymentMsg
    )


type alias ComputedModel =
    { expenses : Expenses
    , debts : Expenses
    , balance : Dict Int Float
    , suggestedPayments : Dict Int (List ( Int, Float ))
    }


type SummaryPerspective
    = SummaryPerspectiveOutlays
    | SummaryPerspectiveDebt


type Msg
    = SetSummaryPerspective SummaryPerspective
    | Disable
    | Enable (List Int) (List Expense)
    | PaymentMsg Payment.Msg


view : Participant.Model -> Model -> Html Msg
view participantModel model =
    row
        [ div [ Html.Attributes.class "col" ] <|
            List.concat
                [ [ Html.h3 [] [ text "Balances" ]
                  , viewBalances participantModel.idToName model
                  ]
                , [ Html.h3 [] [ text "Payments" ]
                  ]
                , Payment.view participantModel model.payment |> List.map (Html.map PaymentMsg)
                ]
        , div [ Html.Attributes.class "col-4" ]
            [ div [ Html.Attributes.class "card" ]
                [ div [ Html.Attributes.class "card-header" ]
                    [ text "Summary (before payments)" ]
                , div [ Html.Attributes.class "card-body" ] <|
                    viewSummary participantModel.idToName model
                ]
            ]
        ]


viewSummary : Dict Int String -> Model -> List (Html Msg)
viewSummary participants model =
    [ div [ Html.Attributes.class "form-check form-check-inline" ]
        [ Html.input
            [ Html.Attributes.class "form-check-input"
            , Html.Attributes.id "computation-summary-outlay"
            , Html.Attributes.name "computation-summary"
            , Html.Attributes.type_ "radio"
            , Html.Attributes.checked (model.summaryPerspective == SummaryPerspectiveOutlays)
            , Html.Events.onCheck (always <| SetSummaryPerspective SummaryPerspectiveOutlays)
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
            , Html.Attributes.checked (model.summaryPerspective == SummaryPerspectiveDebt)
            , Html.Events.onCheck (always <| SetSummaryPerspective SummaryPerspectiveDebt)
            ]
            []
        , Html.label
            [ Html.Attributes.class "form-check-label"
            , Html.Attributes.for "computation-summary-debt"
            ]
            [ text "Debt" ]
        ]
    , Html.hr [] []
    , case model.computed of
        Nothing ->
            Html.p [] [ Html.em [] [ text "No result available yet." ] ]

        Just computed ->
            viewSummaryList participants model.summaryPerspective computed
    ]


viewSummaryList : Dict Int String -> SummaryPerspective -> ComputedModel -> Html Msg
viewSummaryList participants perspective computed =
    case perspective of
        SummaryPerspectiveOutlays ->
            if computed.expenses |> Dict.isEmpty then
                Html.p [] [ Html.i [] [ text "None." ] ]

            else
                Html.ul []
                    (computed.expenses
                        |> Dict.toFlatList
                        |> List.map
                            (\( payer, receiver, amount ) ->
                                ( participants |> lookupName payer
                                , participants |> lookupName receiver
                                , amount |> String.fromAmount
                                )
                            )
                        |> List.sort
                        |> List.map
                            (\( payer, receiver, amount ) ->
                                Html.li []
                                    [ text <|
                                        payer
                                            ++ " has expended "
                                            ++ amount
                                            ++ " for "
                                            ++ receiver
                                            ++ "."
                                    ]
                            )
                    )

        SummaryPerspectiveDebt ->
            if computed.debts |> Dict.isEmpty then
                Html.p [] [ Html.i [] [ text "None." ] ]

            else
                Html.ul []
                    (computed.debts
                        |> Dict.toFlatList
                        |> List.map
                            (\( receiver, payer, amount ) ->
                                ( lookupName receiver participants
                                , lookupName payer participants
                                , amount |> String.fromAmount
                                )
                            )
                        |> List.sort
                        |> List.map
                            (\( receiver, payer, amount ) ->
                                Html.li []
                                    [ text <|
                                        receiver
                                            ++ " owes "
                                            ++ payer
                                            ++ " "
                                            ++ amount
                                            ++ "."
                                    ]
                            )
                    )


viewBalances : Dict Int String -> Model -> Html Msg
viewBalances participants model =
    Html.table [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "Participant" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Balance" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Suggested payments" ]
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
                                        , Html.td [] [ text (amount |> String.fromAmount) ]
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
                                                                    [ Html.a
                                                                        [ Html.Attributes.href "#"
                                                                        , Html.Events.preventDefaultOn "click" <|
                                                                            Json.Decode.succeed ( Payment.ApplySuggestedPayment participantId receiverId suggestedAmount |> PaymentMsg, True )
                                                                        ]
                                                                        [ Html.text <|
                                                                            "Pay "
                                                                                ++ String.fromAmount suggestedAmount
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
    Dict Int (Dict Int Float)


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
                            (\receiver amount innerResult ->
                                if receiver == expense.payer then
                                    -- Ignore debt to self.
                                    innerResult

                                else
                                    innerResult
                                        |> Dict.insert receiver (amount * expense.amount / weightSum)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSummaryPerspective value ->
            ( { model | summaryPerspective = value }, Cmd.none )

        Disable ->
            ( { model | computed = Nothing }, Cmd.none )

        Enable participantIds expenseList ->
            -- TODO Instead of enable/disable, detect if the expense list actually changed and only recompute if it did.
            --      If only the participant list changed, just add/remove the relevant balance entries.
            if Maybe.isJust model.computed then
                ( model, Cmd.none )

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
                ( { model
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
                , Cmd.none
                )

        PaymentMsg paymentMsg ->
            let
                ( ( newPaymentModel, recompute ), newPaymentCmd ) =
                    model.payment |> Payment.update (model.computed |> Maybe.map .balance) paymentMsg
            in
            ( { model
                | payment = newPaymentModel
                , computed =
                    if recompute then
                        model.computed
                            |> Maybe.map
                                (\computed ->
                                    { computed
                                        | suggestedPayments =
                                            Payment.autosuggestPayments (Dict.sumValues computed.balance newPaymentModel.paymentBalance)
                                    }
                                )

                    else
                        model.computed
              }
            , newPaymentCmd |> Cmd.map PaymentMsg
            )
