module Payment exposing (..)

import Amount exposing (Amount)
import Browser.Dom as Dom
import Config exposing (Config)
import Dict exposing (Dict)
import Domain exposing (Payment, lookupBalance, normalizePayment, suggestPaymentAmount, updatePaymentBalances)
import Expense
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Layout exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Participant
import Task
import Util.Number as Number
import Util.Update as Update


createModalId =
    "payment-create"


createModalOpenId =
    createModalId ++ "-open"


type alias Model =
    { create : Maybe CreateModel
    , payments : List Payment
    , paymentBalance : Dict Int Amount
    , nextId : Int
    }




import_ : List Payment -> Model -> Model
import_ payments model =
    { model
        | payments = payments
        , paymentBalance =
            payments
                |> List.foldl
                    (\payment -> updatePaymentBalances payment.payer payment.receiver payment.amount)
                    model.paymentBalance
        , nextId =
            1
                + (payments
                    |> List.foldl
                        (\payment -> max payment.id)
                        (model.nextId - 1)
                  )
    }


create : Config -> Int -> CreateModel -> Result String Payment
create config id model =
    -- Should probably run values though their validators...
    case model.payerId |> String.toInt of
        Nothing ->
            Err <| "unexpected non-integer key '" ++ model.payerId ++ "' of payer"

        Just payerId ->
            case model.receiverId |> String.toInt of
                Nothing ->
                    Err <| "unexpected non-integer key '" ++ model.receiverId ++ "' of receiver"

                Just receiverId ->
                    if payerId == receiverId then
                        Err "payer ID must be different from receiver ID"

                    else
                        case model.amount.value |> Amount.fromString config.amount of
                            Nothing ->
                                Err <| "cannot parse amount '" ++ model.amount.value ++ "' as a (floating point) number"

                            Just amount ->
                                Ok
                                    ({ id = id
                                     , payer = payerId
                                     , receiver = receiverId
                                     , amount = amount
                                     , done = model.done
                                     }
                                        |> normalizePayment
                                    )


init : ( Model, Cmd Msg )
init =
    ( { create = Nothing
      , payments = []
      , paymentBalance = Dict.empty
      , nextId = 1
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
    , done = True
    }


type alias CreateModel =
    { payerId : String
    , receiverId : String
    , amount : Validated Field
    , suggestedAmount : Result ( Maybe Int, Maybe Int ) Amount
    , done : Bool
    }


{-| Tuple of receiver ID, amount, and ID of existing payment to amend.
The payment ID is represented as a pair of the ID and a boolean indicating
whether the amount should be added (false) or subtracted (true).
-}
type alias SuggestedPayment =
    ( Int, Amount, Maybe ( Int, Bool ) )


type Msg
    = LoadCreate (List Int)
    | CloseModal
    | CreateEditPayer String
    | CreateEditReceiver String
    | CreateEditAmount String
    | CreateSetDone Bool
    | CreateApplySuggestedAmount Amount
    | CreateSubmit
    | Delete (List Int)
    | ApplySuggestedPayments (Dict Int (List SuggestedPayment))
    | SetDone Int Bool
    | LayoutMsg Layout.Msg
    | DomMsg (Result Dom.Error ())


view : Config -> Participant.Model -> Model -> List (Html Msg)
view config participantModel model =
    [ Html.table [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "#" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Receiver" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ]
                , Html.th [ Html.Attributes.scope "col" ] <|
                    [ Html.text "Done"
                    , let
                        plannedPayments =
                            model.payments |> List.filterNot .done
                      in
                      Html.button
                        [ Html.Attributes.class <|
                            "ms-1 badge btn btn-primary"
                                ++ (if plannedPayments |> List.isEmpty then
                                        " invisible"

                                    else
                                        ""
                                   )
                        , Html.Events.onClick <| Delete (plannedPayments |> List.map .id)
                        ]
                        [ Html.text "delete all planned" ]
                    ]
                , Html.th [] []
                ]
            ]
        , Html.Keyed.node "tbody"
            []
            (model.payments
                |> List.map
                    (\payment ->
                        let
                            id =
                                payment.id |> String.fromInt
                        in
                        ( id
                        , Html.tr []
                            [ Html.td [] [ Html.text id ]
                            , Html.td [] [ Html.text (participantModel.idToName |> Participant.lookupName payment.payer) ]
                            , Html.td [] [ Html.text (participantModel.idToName |> Participant.lookupName payment.receiver) ]
                            , Html.td [] [ Html.text (payment.amount |> Amount.toString config.amount) ]
                            , Html.td []
                                [ Html.input
                                    [ Html.Attributes.type_ "checkbox"
                                    , Html.Attributes.class "form-check-input"
                                    , Html.Attributes.checked payment.done
                                    , Html.Events.onCheck (SetDone payment.id)
                                    ]
                                    []
                                ]
                            , Html.td
                                [ Html.Attributes.align "right" ]
                                [ Html.a
                                    [ data "bs-toggle" "tooltip"
                                    , data "bs-placement" "left"
                                    , Html.Attributes.class <|
                                        "text-reset"
                                            ++ (if payment.done then
                                                    " invisible"

                                                else
                                                    ""
                                               )
                                    , Html.Attributes.title "Delete"
                                    , Html.Attributes.attribute "role" "button"
                                    , Html.Events.onClick <| Delete [ payment.id ]
                                    ]
                                    [ Html.i [ Html.Attributes.class "bi bi-trash" ] [] ]
                                ]
                            ]
                        )
                    )
            )
        ]
    , viewCreateOpen participantModel
    , viewCreateModal config participantModel model
    ]


viewCreateOpen : Participant.Model -> Html Msg
viewCreateOpen participantModel =
    openModalButton
        createModalOpenId
        createModalId
        "Add payment"
        [ Html.Attributes.class "w-100"
        , Html.Attributes.disabled (participantModel.participants |> List.isEmpty)
        , Html.Events.onClick (participantModel.participants |> List.map .id |> LoadCreate)
        ]


viewCreateModal : Config -> Participant.Model -> Model -> Html Msg
viewCreateModal config participantModel model =
    let
        ( body, disable ) =
            case model.create of
                Nothing ->
                    ( [ Html.text "Loading..." ], True )

                Just createModel ->
                    ( viewAdd config participantModel createModel
                    , String.isEmpty createModel.amount.value
                        || createModel.payerId
                        == createModel.receiverId
                        || List.any isInvalid [ createModel.amount ]
                    )
    in
    Html.form
        [ Html.Events.onSubmit CreateSubmit ]
        [ modal createModalId "Add payment" body disable Nothing ]


viewAdd : Config -> Participant.Model -> CreateModel -> List (Html Msg)
viewAdd config participantModel model =
    let
        participantsFields =
            participantModel.participants
                |> List.map Participant.toField

        ( payerFeedback, receiverFeedback, suggestedAmount ) =
            if model.payerId == model.receiverId then
                ( None, Error "Receiver must be different from payer.", Nothing )

            else
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
                        , if amount == (model.amount.value |> Amount.fromString config.amount |> Maybe.withDefault 0) then
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
        CreateEditPayer
    , optionsInput "new-payments-receiver"
        "Receiver"
        { fields = participantsFields, feedback = receiverFeedback }
        model.receiverId
        CreateEditReceiver
    , div
        ([ Html.Attributes.class "row mb-3" ]
            ++ (if Maybe.isNothing suggestedAmount then
                    [ Html.Attributes.class "d-none" ]

                else
                    []
               )
        )
        [ div [ Html.Attributes.class "col-sm-3" ] []
        , div [ Html.Attributes.class "col-sm-9" ] <|
            case suggestedAmount of
                Nothing ->
                    []

                Just amount ->
                    [ Layout.internalLink
                        (CreateApplySuggestedAmount amount)
                        [ text <| "Balance difference: " ++ (amount |> Amount.toString config.amount) ]
                    ]
        ]
    , textInput "Amount" model.amount CreateEditAmount
    , Html.fieldset [ Html.Attributes.class "row mb-3" ]
        [ Html.label
            [ Html.Attributes.class "col-form-label col-sm-3 pt-0"
            , Html.Attributes.for "new-payments-done"
            ]
            [ text "Done" ]
        , div [ Html.Attributes.class "col-sm-9" ]
            [ div [ Html.Attributes.class "form-check" ]
                [ Html.input
                    [ Html.Attributes.id "new-payments-done"
                    , Html.Attributes.type_ "checkbox"
                    , Html.Attributes.class "form-check-input"
                    , Html.Attributes.checked model.done
                    , Html.Events.onCheck CreateSetDone
                    ]
                    []
                ]
            ]
        ]
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    modalClosed ModalClosed |> Sub.map LayoutMsg


update : Config -> Maybe (Dict Int Amount) -> Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update config balances msg model =
    case msg of
        LoadCreate participants ->
            let
                (( firstParticipantFallback, secondParticipantFallback ) as fallbackResult) =
                    case participants of
                        firstParticipant :: secondParticipant :: _ ->
                            ( Just firstParticipant, Just secondParticipant )

                        _ ->
                            ( Nothing, Nothing )

                ( firstNegativeBalanceParticipant, firstPositiveBalanceParticipant ) =
                    case balances of
                        Nothing ->
                            fallbackResult

                        Just participantBalances ->
                            participantBalances
                                |> Dict.foldl
                                    (\participantId participantBalance ( negativeResult, positiveResult ) ->
                                        let
                                            totalBalance =
                                                participantBalance + lookupBalance participantId model.paymentBalance

                                            updateResult isValid result =
                                                case result of
                                                    Nothing ->
                                                        if isValid totalBalance then
                                                            Just participantId

                                                        else
                                                            Nothing

                                                    Just _ ->
                                                        result
                                        in
                                        ( updateResult Number.isNegative negativeResult
                                        , updateResult Number.isPositive positiveResult
                                        )
                                    )
                                    ( Nothing, Nothing )
            in
            ( ( { model
                    | create =
                        participants
                            |> List.head
                            |> Maybe.map
                                (\firstParticipant ->
                                    initCreate (firstParticipant |> String.fromInt)
                                )
                }
              , False
              )
            , Cmd.none
            )
                |> Update.chains (Update.withPairModel (update config balances) (||))
                    ((case firstNegativeBalanceParticipant |> Maybe.orElse firstParticipantFallback of
                        Nothing ->
                            []

                        Just payer ->
                            [ CreateEditPayer (payer |> String.fromInt) ]
                     )
                        ++ (case firstPositiveBalanceParticipant |> Maybe.orElse secondParticipantFallback of
                                Nothing ->
                                    []

                                Just receiver ->
                                    [ CreateEditReceiver (receiver |> String.fromInt) ]
                           )
                    )

        CloseModal ->
            ( ( model, False ), closeModal createModalId )

        CreateEditPayer payerId ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel ->
                                    { createModel
                                        | payerId = payerId
                                        , suggestedAmount =
                                            balances
                                                |> Maybe.unwrap (Err ( Nothing, Nothing ))
                                                    (suggestPaymentAmount
                                                        payerId
                                                        createModel.receiverId
                                                        model.paymentBalance
                                                    )
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateEditReceiver receiverId ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel ->
                                    { createModel
                                        | receiverId = receiverId
                                        , suggestedAmount =
                                            balances
                                                |> Maybe.unwrap (Err ( Nothing, Nothing ))
                                                    (suggestPaymentAmount
                                                        createModel.payerId
                                                        receiverId
                                                        model.paymentBalance
                                                    )
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateEditAmount amount ->
            ( ( { model
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
                                                , feedback = Expense.validateAmountInput config.amount validatePaymentAmount amount
                                            }
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateApplySuggestedAmount amount ->
            ( model
            , case model.create of
                Nothing ->
                    Cmd.none

                Just createModel ->
                    createModel.amount.key |> Dom.focus |> Task.attempt DomMsg
            )
                |> Update.chain
                    (update config balances)
                    (amount |> Amount.toString config.amount |> CreateEditAmount)

        CreateSetDone done ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel -> { createModel | done = done })
                }
              , False
              )
            , Cmd.none
            )

        CreateSubmit ->
            let
                id =
                    model.nextId

                result =
                    model.create
                        |> Result.fromMaybe "no create model found"
                        |> Result.andThen (create config id)
            in
            case result of
                Err error ->
                    -- TODO Print error on page.
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( ( model, False ), Cmd.none )

                Ok payment ->
                    ( ( { model
                            | payments =
                                model.payments ++ [ payment ]
                            , paymentBalance =
                                model.paymentBalance
                                    |> updatePaymentBalances payment.payer
                                        payment.receiver
                                        payment.amount
                            , nextId = id + 1
                        }
                      , True
                      )
                    , Update.delegate CloseModal
                    )

        Delete paymentIds ->
            let
                ( deletedPayments, retainedPayments ) =
                    model.payments
                        |> List.partition (\payment -> List.member payment.id paymentIds)
            in
            ( ( { model
                    | payments = retainedPayments
                    , paymentBalance =
                        deletedPayments
                            |> List.foldl
                                (\deletedPayment ->
                                    updatePaymentBalances deletedPayment.receiver deletedPayment.payer deletedPayment.amount
                                )
                                model.paymentBalance
                }
              , True
              )
            , createModalOpenId |> Dom.focus |> Task.attempt DomMsg
            )

        ApplySuggestedPayments suggestedPayments ->
            let
                modelResult =
                    suggestedPayments
                        |> Dict.foldl
                            (\payerId payerSuggestedPayments outerModelResult ->
                                payerSuggestedPayments
                                    |> List.foldl
                                        (\( receiverId, amount, existingPaymentId ) innerModelResult ->
                                            case existingPaymentId of
                                                Nothing ->
                                                    { innerModelResult
                                                        | payments =
                                                            innerModelResult.payments
                                                                ++ [ { id = innerModelResult.nextId
                                                                     , payer = payerId
                                                                     , receiver = receiverId
                                                                     , amount = amount
                                                                     , done = False
                                                                     }
                                                                        |> normalizePayment
                                                                   ]
                                                        , paymentBalance =
                                                            innerModelResult.paymentBalance
                                                                |> updatePaymentBalances
                                                                    payerId
                                                                    receiverId
                                                                    amount
                                                        , nextId = innerModelResult.nextId + 1
                                                    }

                                                Just ( paymentId, inverse ) ->
                                                    { innerModelResult
                                                        | payments =
                                                            innerModelResult.payments
                                                                |> List.updateIf (.id >> (==) paymentId)
                                                                    (\payment ->
                                                                        { payment
                                                                            | amount =
                                                                                if inverse then
                                                                                    payment.amount - amount

                                                                                else
                                                                                    payment.amount + amount
                                                                        }
                                                                            |> normalizePayment
                                                                    )
                                                        , paymentBalance =
                                                            innerModelResult.paymentBalance
                                                                |> updatePaymentBalances
                                                                    payerId
                                                                    receiverId
                                                                    amount
                                                        , nextId = innerModelResult.nextId + 1
                                                    }
                                        )
                                        outerModelResult
                            )
                            model
            in
            ( ( modelResult, True ), Cmd.none )

        SetDone paymentId done ->
            ( ( { model
                    | payments =
                        model.payments
                            |> List.updateIf (.id >> (==) paymentId) (\payment -> { payment | done = done })
                }
              , True
              )
            , Cmd.none
            )

        LayoutMsg layoutMsg ->
            case layoutMsg of
                -- Must explicitly reset the modal for Firefox to render selects correctly on next open.
                ModalClosed modalId ->
                    if modalId == createModalId then
                        ( ( { model | create = Nothing }, False ), Cmd.none )

                    else
                        ( ( model, False ), Cmd.none )

        DomMsg result ->
            let
                _ =
                    case result of
                        Err (Dom.NotFound id) ->
                            Debug.log "DOM error: Element not found" id

                        Ok () ->
                            ""
            in
            ( ( model, False ), Cmd.none )




validatePaymentAmount : Amount -> Feedback
validatePaymentAmount amount =
    if amount < 0 then
        Info "Payments with negative amounts will swap payer and receiver."

    else
        None
