module Computation exposing (..)

import Dict exposing (Dict)
import Expense exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Layout exposing (..)
import Maybe.Extra as Maybe
import Participant exposing (lookupName)
import Util.Dict as Dict
import Util.String as String
import Util.Update as Update


createModalId =
    "payment-create"


type alias Model =
    { create : Maybe CreateModel
    , summaryPerspective : SummaryPerspective
    , computed : Maybe ComputedModel
    , payments : List Payment
    , paymentBalance : Dict Int Float
    , nextPaymentId : Int
    }


type alias Payment =
    { id : String
    , payer : Int
    , receiver : Int
    , amount : Float
    }


create : Int -> CreateModel -> Result String Payment
create id model =
    -- Should probably run values though their validators...
    let
        payerResult =
            case model.payerId |> String.toInt of
                Nothing ->
                    Err ("unexpected non-integer key '" ++ model.payerId ++ "' of payer")

                Just payerId ->
                    Ok payerId

        receiverResult =
            case model.receiverId |> String.toInt of
                Nothing ->
                    Err ("unexpected non-integer key '" ++ model.receiverId ++ "' of receiver")

                Just payerId ->
                    Ok payerId

        amountResult =
            case model.amount.value |> String.toFloat of
                Nothing ->
                    Err ("cannot parse amount '" ++ model.amount.value ++ "' as a (floating point) number")

                Just amount ->
                    Ok amount
    in
    Result.map3
        (\payerId receiverId amount ->
            { id = id |> String.fromInt
            , payer = payerId
            , receiver = receiverId
            , amount = amount
            }
        )
        payerResult
        receiverResult
        amountResult


init : Model
init =
    { create = Nothing
    , summaryPerspective = SummaryPerspectiveOutlays
    , computed = Nothing
    , payments = []
    , paymentBalance = Dict.empty
    , nextPaymentId = 1
    }


initCreate : String -> CreateModel
initCreate initId =
    { payerId = initId
    , receiverId = initId
    , amount =
        { key = "payment-create-amount"
        , value = ""
        , validationError = Nothing
        }
    , suggestedAmount = Nothing
    }


type alias ComputedModel =
    { expenses : Expenses
    , debts : Expenses
    , balance : Dict Int Float
    }


type alias CreateModel =
    { payerId : String
    , receiverId : String
    , amount : Validated Field
    , suggestedAmount : Maybe Float
    }


type SummaryPerspective
    = SummaryPerspectiveOutlays
    | SummaryPerspectiveDebt


type Msg
    = SetSummaryPerspective SummaryPerspective
    | RecomputeBalance (List Int) (List Expense)
    | LoadCreate (List Int)
    | CloseModal
    | CreatePaymentEditPayer String
    | CreatePaymentEditReceiver String
    | CreatePaymentEditAmount String
    | CreatePaymentSubmit


view : Participant.Model -> Model -> Html Msg
view participantModel model =
    row
        [ div [ Html.Attributes.class "col" ]
            [ Html.h3 [] [ text "Payments" ]
            , viewPayments participantModel model
            , Html.h3 [] [ text "Balances" ]
            , viewBalances participantModel.idToName model
            ]
        , div [ Html.Attributes.class "col-4" ]
            [ div [ Html.Attributes.class "card" ]
                [ div [ Html.Attributes.class "card-header" ]
                    [ text "Summary (before payments)" ]
                , div [ Html.Attributes.class "card-body" ]
                    [ viewSummary participantModel.idToName model ]
                ]
            ]
        ]


viewPayments : Participant.Model -> Model -> Html Msg
viewPayments participantModel model =
    div [] <|
        [ Html.table [ Html.Attributes.class "table" ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "#" ]
                    , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ]
                    , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Receiver" ]
                    , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ]
                    ]
                ]
            , Html.Keyed.node "tbody"
                []
                (model.payments
                    |> List.map
                        (\payment ->
                            ( payment.id
                            , Html.tr []
                                [ Html.td [] [ Html.text payment.id ]
                                , Html.td [] [ Html.text (participantModel.idToName |> Participant.lookupName payment.payer) ]
                                , Html.td [] [ Html.text (participantModel.idToName |> Participant.lookupName payment.receiver) ]
                                , Html.td [] [ Html.text (payment.amount |> String.fromAmount) ]
                                ]
                            )
                        )
                )
            ]
        , viewCreateOpen participantModel
        , viewCreateModal participantModel model
        ]


viewCreateOpen : Participant.Model -> Html Msg
viewCreateOpen participantModel =
    openModalButton
        (createModalId ++ "-open")
        createModalId
        "Add payment"
        [ Html.Attributes.disabled (participantModel.participants |> List.isEmpty)
        , Html.Events.onClick (participantModel.participants |> List.map .id |> LoadCreate)
        ]


viewCreateModal : Participant.Model -> Model -> Html Msg
viewCreateModal participantModel model =
    let
        ( body, disable ) =
            case model.create of
                Nothing ->
                    ( [ Html.text "Loading..." ], True )

                Just createModel ->
                    ( viewAdd participantModel createModel
                    , String.isEmpty createModel.amount.value
                        || List.any isInvalid [ createModel.amount ]
                    )
    in
    Html.form
        [ Html.Events.onSubmit CreatePaymentSubmit ]
        [ modal createModalId "Add payment" body disable ]


viewAdd : Participant.Model -> CreateModel -> List (Html Msg)
viewAdd participantModel model =
    let
        participantsFields =
            participantModel.participants
                |> List.map Participant.toField
    in
    [ optionsInput "new-payments-payer" "Payer" participantsFields model.payerId CreatePaymentEditPayer
    , optionsInput "new-payments-receiver" "Receiver" participantsFields model.receiverId CreatePaymentEditReceiver
    , textInput "Amount" model.amount CreatePaymentEditAmount
    , div [ Html.Attributes.class "row mb-3" ]
        [ Html.div [ Html.Attributes.class "col-sm-3" ] []
        , div [ Html.Attributes.class "col-sm-9" ]
            [ text <| "Suggestested: "
            , Html.a [] [ text <| (model.suggestedAmount |> Maybe.withDefault 0 |> String.fromAmount) ]
            ]
        ]
    ]


viewSummary : Dict Int String -> Model -> Html Msg
viewSummary participants model =
    Html.div []
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
                                    [ text <| payer ++ " has expended " ++ amount ++ " for " ++ receiver ++ "." ]
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
                                Html.li [] [ text (receiver ++ " owes " ++ payer ++ " " ++ amount ++ ".") ]
                            )
                    )


viewBalances : Dict Int String -> Model -> Html Msg
viewBalances participants model =
    Html.table [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "Participant" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Balance" ]
                ]
            ]
        , Html.Keyed.node "tbody"
            []
            (model.computed
                |> Maybe.unwrap []
                    (.balance
                        >> Dict.toList
                        >> List.map
                            (\( participantId, expendedAmount ) ->
                                let
                                    participantName =
                                        lookupName participantId participants

                                    paymentBalance =
                                        model.paymentBalance
                                            |> Dict.get participantId
                                            |> Maybe.withDefault 0

                                    totalBalance =
                                        expendedAmount + paymentBalance
                                in
                                ( participantName
                                , participantId
                                , totalBalance |> String.fromAmountSigned
                                )
                            )
                        -- Sort by name, then ID.
                        >> List.sort
                        >> List.map
                            (\( participantName, participantId, amount ) ->
                                ( participantId |> String.fromInt
                                , Html.tr [] [ Html.td [] [ text participantName ], Html.td [] [ text amount ] ]
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


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        SetSummaryPerspective value ->
            ( { model | summaryPerspective = value }, Cmd.none )

        RecomputeBalance participantIds expenseList ->
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
                        }
              }
            , Cmd.none
            )

        LoadCreate participants ->
            ( { model
                | create =
                    participants
                        |> List.head
                        |> Maybe.map
                            (\firstParticipant ->
                                initCreate (firstParticipant |> String.fromInt)
                            )
              }
            , Cmd.none
            )

        CloseModal ->
            ( model, closeModal createModalId )

        CreatePaymentEditPayer payerId ->
            ( { model
                | create =
                    model.create
                        |> Maybe.map
                            (\createModel ->
                                { createModel
                                    | payerId = payerId
                                    , suggestedAmount =
                                        model.computed
                                            |> Maybe.map
                                                (.balance
                                                    >> suggestPaymentAmount
                                                        payerId
                                                        createModel.receiverId
                                                        model.paymentBalance
                                                )
                                }
                            )
              }
            , Cmd.none
            )

        CreatePaymentEditReceiver receiverId ->
            ( { model
                | create =
                    model.create
                        |> Maybe.map
                            (\createModel ->
                                { createModel
                                    | receiverId = receiverId
                                    , suggestedAmount =
                                        model.computed
                                            |> Maybe.map
                                                (.balance
                                                    >> suggestPaymentAmount
                                                        createModel.payerId
                                                        receiverId
                                                        model.paymentBalance
                                                )
                                }
                            )
              }
            , Cmd.none
            )

        CreatePaymentEditAmount amount ->
            ( { model
                | create =
                    model.create
                        |> Maybe.map
                            (\createModel ->
                                let
                                    amountField =
                                        createModel.amount
                                in
                                { createModel
                                    | amount =
                                        { amountField
                                            | value = amount
                                            , validationError = Expense.validateAmount amount
                                        }
                                }
                            )
              }
            , Cmd.none
            )

        CreatePaymentSubmit ->
            let
                id =
                    model.nextPaymentId

                result =
                    model.create
                        |> Result.fromMaybe "no create model found"
                        |> Result.andThen (create id)
            in
            case result of
                Err error ->
                    -- TODO Print error on page.
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )

                Ok payment ->
                    ( { model
                        | payments =
                            model.payments ++ [ payment ]
                        , paymentBalance =
                            model.paymentBalance
                                |> Dict.update payment.payer
                                    (Maybe.withDefault 0
                                        >> (\payerBalance -> payerBalance + payment.amount)
                                        >> Just
                                    )
                                |> Dict.update payment.receiver
                                    (Maybe.withDefault 0
                                        >> (\receiverBalance -> receiverBalance - payment.amount)
                                        >> Just
                                    )
                        , nextPaymentId = id + 1
                      }
                    , Update.delegate CloseModal
                    )


suggestPaymentAmount : String -> String -> Dict Int number -> Dict Int number -> number
suggestPaymentAmount payer receiver paymentBalance balance =
    let
        payerId =
            payer |> String.toInt |> Maybe.withDefault 0

        receiverId =
            receiver |> String.toInt |> Maybe.withDefault 0

        payerBalance =
            (balance |> Dict.get payerId |> Maybe.withDefault 0)
                + (paymentBalance |> Dict.get payerId |> Maybe.withDefault 0)

        receiverBalance =
            (balance |> Dict.get receiverId |> Maybe.withDefault 0)
                + (paymentBalance |> Dict.get receiverId |> Maybe.withDefault 0)
    in
    min -payerBalance receiverBalance
        |> max 0
