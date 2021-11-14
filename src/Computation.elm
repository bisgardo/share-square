module Computation exposing (..)

import Browser.Dom as Dom
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
import Task
import Util.Dict as Dict
import Util.List as List
import Util.String as String
import Util.Update as Update



-- TODO Move "payment" things to it's own component.


createModalId =
    "payment-create"


createModalOpenId =
    createModalId ++ "-open"


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


init : ( Model, Cmd Msg )
init =
    ( { create = Nothing
      , summaryPerspective = SummaryPerspectiveOutlays
      , computed = Nothing
      , payments = []
      , paymentBalance = Dict.empty
      , nextPaymentId = 1
      }
    , Cmd.none
    )


initCreate : String -> CreateModel
initCreate initId =
    { payerId = initId
    , receiverId = initId
    , amount =
        { key = "payment-create-amount"
        , value = ""
        , feedback = None
        }
    , suggestedAmount = Err ( Nothing, Nothing )
    }


type alias ComputedModel =
    { expenses : Expenses
    , debts : Expenses
    , balance : Dict Int Float
    , suggestedPayments : Dict Int (List ( Int, Float ))
    }


type alias CreateModel =
    { payerId : String
    , receiverId : String
    , amount : Validated Field
    , suggestedAmount : Result ( Maybe Int, Maybe Int ) Float
    }


type SummaryPerspective
    = SummaryPerspectiveOutlays
    | SummaryPerspectiveDebt


type Msg
    = SetSummaryPerspective SummaryPerspective
    | DisableComputation
    | EnableComputation (List Int) (List Expense)
    | LoadCreate (List Int)
    | CloseModal
    | CreatePaymentEditPayer String
    | CreatePaymentEditReceiver String
    | CreatePaymentEditAmount String
    | CreatePaymentApplySuggestedAmount Float
    | CreatePaymentSubmit
    | DeletePayment String
    | ApplySuggestedPayment Int Int Float
    | LayoutMsg Layout.Msg
    | DomMsg (Result Dom.Error ())


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
                , viewPayments participantModel model
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



-- TODO Move down.


viewPayments : Participant.Model -> Model -> List (Html Msg)
viewPayments participantModel model =
    [ Html.table [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "#" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Receiver" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ]
                , Html.th [] []
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
                            , Html.td
                                [ Html.Attributes.align "right", Html.Events.onClick (DeletePayment payment.id) ]
                                [ Html.a
                                    [ data "bs-toggle" "tooltip"
                                    , data "bs-placement" "left"
                                    , Html.Attributes.title "Delete"
                                    , Html.Attributes.attribute "role" "button"
                                    ]
                                    [ Html.i [ Html.Attributes.class "bi bi-trash" ] [] ]
                                ]
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
        createModalOpenId
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

        ( payerFeedback, receiverFeedback, suggestedAmount ) =
            case model.suggestedAmount of
                Err ( payerIdNotOwing, receiverIdNotOwed ) ->
                    ( payerIdNotOwing
                        |> Maybe.unwrap None
                            (\payerId ->
                                Info <|
                                    (participantModel.idToName |> Participant.lookupName payerId)
                                        ++ " doesn't owe anything."
                            )
                    , receiverIdNotOwed
                        |> Maybe.unwrap None
                            (\receiverId ->
                                Info <|
                                    (participantModel.idToName |> Participant.lookupName receiverId)
                                        ++ " isn't owed anything."
                            )
                    , Nothing
                    )

                Ok amount ->
                    ( None
                    , None
                    , if amount == (model.amount.value |> String.toFloat |> Maybe.withDefault 0) then
                        -- Amount is already the suggested value.
                        Nothing

                      else
                        Just amount
                    )
    in
    [ optionsInput "new-payments-payer"
        "Payer"
        { fields = participantsFields, feedback = payerFeedback }
        model.payerId
        CreatePaymentEditPayer
    , optionsInput "new-payments-receiver"
        "Receiver"
        { fields = participantsFields, feedback = receiverFeedback }
        model.receiverId
        CreatePaymentEditReceiver
    , div
        ([ Html.Attributes.class "row mb-3" ]
            ++ (if Maybe.isNothing suggestedAmount then
                    [ Html.Attributes.class "d-none" ]

                else
                    []
               )
        )
        [ Html.div [ Html.Attributes.class "col-sm-3" ] []
        , div [ Html.Attributes.class "col-sm-9" ] <|
            case suggestedAmount of
                Nothing ->
                    []

                Just amount ->
                    [ Html.a
                        -- Need to set href "#" to render the link correctly,
                        -- but then "prevent default" also needs to be enabled to actually prevent a URL change.
                        -- I'm amazed that JSON decoding must be involved to do this, but it seems to be the case...
                        [ Html.Attributes.href "#"
                        , Html.Events.preventDefaultOn "click" <|
                            Json.Decode.succeed ( CreatePaymentApplySuggestedAmount amount, True )
                        ]
                        [ text <| "Suggestested amount: " ++ (amount |> String.fromAmount) ]
                    ]
        ]
    , textInput "Amount" model.amount CreatePaymentEditAmount
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
                                    , Html.tr []
                                        [ Html.td [] [ text participantName ]
                                        , Html.td [] [ text amount ]
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
                                                                            Json.Decode.succeed ( ApplySuggestedPayment participantId receiverId suggestedAmount, True )
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
subscriptions _ =
    modalClosed ModalClosed |> Sub.map LayoutMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSummaryPerspective value ->
            ( { model | summaryPerspective = value }, Cmd.none )

        DisableComputation ->
            ( { model | computed = Nothing }, Cmd.none )

        EnableComputation participantIds expenseList ->
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
                                autosuggestPayments (Dict.sumValues balance model.paymentBalance)
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
                                            |> Maybe.unwrap (Err ( Nothing, Nothing ))
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
                                            |> Maybe.unwrap (Err ( Nothing, Nothing ))
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
                                            , feedback = Expense.validateAmount amount
                                        }
                                }
                            )
              }
            , Cmd.none
            )

        CreatePaymentApplySuggestedAmount amount ->
            ( model
            , case model.create of
                Nothing ->
                    Cmd.none

                Just createModel ->
                    Dom.focus createModel.amount.key |> Task.attempt DomMsg
            )
                |> Update.chain
                    (amount |> String.fromAmount |> CreatePaymentEditAmount)
                    update

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
                    ( model |> addPayment payment (id + 1)
                    , Update.delegate CloseModal
                    )

        DeletePayment paymentId ->
            case model.payments |> List.withoutFirstMatch (.id >> (==) paymentId) of
                ( Nothing, _ ) ->
                    -- Should never happen.
                    let
                        _ =
                            Debug.log "error" <| "Cannot delete payment with non-existent ID '" ++ paymentId ++ "'."
                    in
                    ( model, Cmd.none )

                ( Just payment, newPayments ) ->
                    let
                        paymentBalance =
                            model.paymentBalance
                                |> updatePaymentBalances payment.receiver payment.payer payment.amount
                    in
                    ( { model
                        | payments = newPayments
                        , paymentBalance = paymentBalance
                        , computed =
                            model.computed
                                |> Maybe.map
                                    (\computed ->
                                        { computed
                                            | suggestedPayments =
                                                autosuggestPayments (Dict.sumValues computed.balance paymentBalance)
                                        }
                                    )
                      }
                    , Dom.focus createModalOpenId |> Task.attempt DomMsg
                    )

        ApplySuggestedPayment payerId receiverId amount ->
            ( model
                |> addPayment
                    { id = model.nextPaymentId |> String.fromInt
                    , payer = payerId
                    , receiver = receiverId
                    , amount = amount
                    }
                    (model.nextPaymentId + 1)
            , Cmd.none
            )

        LayoutMsg layoutMsg ->
            case layoutMsg of
                -- Must explicitly reset the modal for Firefox to render selects correctly on next open.
                ModalClosed modalId ->
                    if modalId == createModalId then
                        ( { model | create = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )

        DomMsg result ->
            let
                _ =
                    case result of
                        Err (Dom.NotFound id) ->
                            Debug.log "DOM error: Element not found" id

                        Ok () ->
                            ""
            in
            ( model, Cmd.none )


addPayment : Payment -> Int -> Model -> Model
addPayment payment nextId model =
    let
        payments =
            model.payments ++ [ payment ]

        paymentBalance =
            model.paymentBalance
                |> updatePaymentBalances payment.payer payment.receiver payment.amount
    in
    { model
        | payments = payments
        , paymentBalance = paymentBalance
        , computed =
            model.computed
                |> Maybe.map
                    (\computed ->
                        { computed
                            | suggestedPayments =
                                autosuggestPayments (Dict.sumValues computed.balance paymentBalance)
                        }
                    )
        , nextPaymentId = nextId
    }


findSuggestedPayment : Dict Int Float -> Maybe ( ( Int, Float ), ( Int, Float ) )
findSuggestedPayment =
    Dict.foldl
        (\participantId participantBalance result ->
            case result of
                Nothing ->
                    Just ( ( participantId, participantBalance ), ( participantId, participantBalance ) )

                Just ( ( _, minAmount ) as minResult, ( _, maxAmount ) as maxResult ) ->
                    if participantBalance < minAmount then
                        Just ( ( participantId, participantBalance ), maxResult )

                    else if participantBalance > maxAmount then
                        Just ( minResult, ( participantId, participantBalance ) )

                    else
                        result
        )
        Nothing


autosuggestPayments : Dict Int Float -> Dict Int (List ( Int, Float ))
autosuggestPayments totalBalances =
    case totalBalances |> autosuggestPayment of
        Nothing ->
            Dict.empty

        Just ( payerId, receiverId, amount ) ->
            totalBalances
                |> updatePaymentBalances payerId receiverId amount
                |> autosuggestPayments
                |> Dict.update payerId (Maybe.withDefault [] >> (::) ( receiverId, amount ) >> Just)


autosuggestPayment : Dict Int Float -> Maybe ( Int, Int, Float )
autosuggestPayment =
    findSuggestedPayment
        >> Maybe.andThen
            (\( ( minParticipant, minBalance ), ( maxParticipant, maxBalance ) ) ->
                let
                    debt =
                        min maxBalance -minBalance
                in
                -- TODO Should really do if |debt| < epsilon?
                if debt == 0 then
                    Nothing

                else
                    Just ( minParticipant, maxParticipant, debt )
            )


sumBalances : Int -> Dict Int Float -> Dict Int Float -> Float
sumBalances participantId paymentBalance balance =
    (balance |> Dict.get participantId |> Maybe.withDefault 0)
        + (paymentBalance |> Dict.get participantId |> Maybe.withDefault 0)


suggestPaymentAmount : String -> String -> Dict Int Float -> Dict Int Float -> Result ( Maybe Int, Maybe Int ) Float
suggestPaymentAmount payer receiver paymentBalance balance =
    let
        payerId =
            payer |> String.toInt |> Maybe.withDefault 0

        receiverId =
            receiver |> String.toInt |> Maybe.withDefault 0

        payerBalance =
            sumBalances payerId paymentBalance balance

        receiverBalance =
            sumBalances receiverId paymentBalance balance

        suggestedAmount =
            min -payerBalance receiverBalance
    in
    if suggestedAmount <= 0 then
        Err
            ( if payerBalance >= 0 then
                Just payerId

              else
                Nothing
            , if receiverBalance <= 0 then
                Just receiverId

              else
                Nothing
            )

    else
        Ok suggestedAmount


updatePaymentBalances : Int -> Int -> Float -> Dict Int Float -> Dict Int Float
updatePaymentBalances payerId receiverId amount =
    updatePaymentBalance payerId amount >> updatePaymentBalance receiverId -amount


updatePaymentBalance : Int -> Float -> Dict Int Float -> Dict Int Float
updatePaymentBalance participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)
