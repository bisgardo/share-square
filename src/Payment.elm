module Payment exposing (..)

import Amount exposing (Amount)
import Browser.Dom as Dom
import Config exposing (Config)
import Dict exposing (Dict)
import Expense exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Layout exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Participant
import Set exposing (Set)
import Task
import Util.JsonDecode as Decode
import Util.List as List
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
    , donePayments : Set Int
    }


type alias Payment =
    { id : Int
    , payer : Int
    , receiver : Int
    , amount : Amount
    }


type alias StorageValues =
    { payments : List Payment
    , done : Set Int
    }


decoder : Decoder StorageValues
decoder =
    Decode.map2
        StorageValues
        (Decode.field "p" <| Decode.nullableList paymentDecoder)
        (Decode.field "d" (Decode.list Decode.int |> Decode.map Set.fromList))


encode : StorageValues -> Value
encode values =
    [ ( "p", values.payments |> Encode.list encodePayment )
    , ( "d", values.done |> Set.toList |> List.sort |> Encode.list Encode.int )
    ]
        |> Encode.object


paymentDecoder : Decoder Payment
paymentDecoder =
    Decode.map4
        (\id payerId amount receiverId ->
            { id = id
            , payer = payerId
            , amount = amount
            , receiver = receiverId
            }
        )
        -- ID
        (Decode.field "i" Decode.int)
        -- payer ID
        (Decode.field "p" Decode.int)
        -- amount
        (Decode.field "a" Amount.decoder)
        -- receiver ID
        (Decode.field "r" Decode.int)


encodePayment : Payment -> Value
encodePayment payment =
    [ ( "i", payment.id |> Encode.int )
    , ( "p", payment.payer |> Encode.int )
    , ( "a", payment.amount |> Amount.encode )
    , ( "r", payment.receiver |> Encode.int )
    ]
        |> Encode.object


import_ : StorageValues -> Model -> Model
import_ values model =
    { model
        | payments = values.payments
        , paymentBalance =
            values.payments
                |> List.foldl
                    (\payment -> updatePaymentBalances payment.payer payment.receiver payment.amount)
                    model.paymentBalance
        , nextId =
            1
                + (values.payments
                    |> List.foldl
                        (\payment -> max payment.id)
                        (model.nextId - 1)
                  )
        , donePayments = values.done
    }


export : Model -> StorageValues
export model =
    { payments = model.payments
    , done = model.donePayments
    }


create : Config -> Int -> CreateModel -> Result String ( Payment, Bool )
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
                                    ( { id = id
                                      , payer = payerId
                                      , receiver = receiverId
                                      , amount = amount
                                      }
                                    , model.done
                                    )


init : ( Model, Cmd Msg )
init =
    ( { create = Nothing
      , payments = []
      , paymentBalance = Dict.empty
      , nextId = 1
      , donePayments = Set.empty
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


type Msg
    = LoadCreate (List Int)
    | CloseModal
    | CreateEditPayer String
    | CreateEditReceiver String
    | CreateEditAmount String
    | CreateSetDone Bool
    | CreateApplySuggestedAmount Amount
    | CreateSubmit
    | Delete Int
    | ApplySuggestedPayments (Dict Int (List ( Int, Amount )))
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
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Done" ]
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
                                    , Html.Attributes.checked (model.donePayments |> Set.member payment.id)
                                    , Html.Events.onCheck (SetDone payment.id)
                                    ]
                                    []
                                ]
                            , Html.td
                                [ Html.Attributes.align "right" ]
                                (if model.donePayments |> Set.member payment.id then
                                    []

                                 else
                                    [ Html.a
                                        [ data "bs-toggle" "tooltip"
                                        , data "bs-placement" "left"
                                        , Html.Attributes.title "Delete"
                                        , Html.Attributes.attribute "role" "button"
                                        , Html.Events.onClick (Delete payment.id)
                                        ]
                                        [ Html.i [ Html.Attributes.class "bi bi-trash" ] [] ]
                                    ]
                                )
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
                                        in
                                        ( case negativeResult of
                                            Nothing ->
                                                if totalBalance < 0 then
                                                    Just participantId

                                                else
                                                    Nothing

                                            Just _ ->
                                                negativeResult
                                        , case positiveResult of
                                            Nothing ->
                                                if totalBalance > 0 then
                                                    Just participantId

                                                else
                                                    Nothing

                                            Just _ ->
                                                positiveResult
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
                                                , feedback = Expense.validateAmount config.amount amount
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

                Ok ( payment, done ) ->
                    ( ( model |> addPayments [ payment ] done (id + 1), True )
                    , Update.delegate CloseModal
                    )

        Delete paymentId ->
            case model.payments |> List.withoutFirstMatch (.id >> (==) paymentId) of
                ( Nothing, _ ) ->
                    -- Should never happen.
                    let
                        _ =
                            Debug.log "error" <| "Cannot delete payment with non-existent ID '" ++ (paymentId |> String.fromInt) ++ "'."
                    in
                    ( ( model, False ), Cmd.none )

                ( Just payment, newPayments ) ->
                    let
                        paymentBalance =
                            model.paymentBalance
                                |> updatePaymentBalances payment.receiver payment.payer payment.amount
                    in
                    ( ( { model
                            | payments = newPayments
                            , paymentBalance = paymentBalance

                            -- Technically needed even though it's currently redundant as deletion isn't exposed to done payments.
                            , donePayments = model.donePayments |> Set.remove paymentId
                        }
                      , True
                      )
                    , createModalOpenId |> Dom.focus |> Task.attempt DomMsg
                    )

        ApplySuggestedPayments suggestedPayments ->
            let
                ( paymentsReversed, nextId ) =
                    suggestedPayments
                        |> Dict.foldl
                            (\payerId payerSuggestedPayments ( payerPaymentsReversed, payerNextId ) ->
                                payerSuggestedPayments
                                    |> List.foldl
                                        (\( receiverId, amount ) ( receiverPayments, receiverNextId ) ->
                                            ( { id = receiverNextId
                                              , payer = payerId
                                              , receiver = receiverId
                                              , amount = amount
                                              }
                                                :: receiverPayments
                                            , receiverNextId + 1
                                            )
                                        )
                                        ( payerPaymentsReversed, payerNextId )
                            )
                            ( [], model.nextId )
            in
            ( ( model |> addPayments (paymentsReversed |> List.reverse) False nextId, True ), Cmd.none )

        SetDone paymentId done ->
            ( ( { model
                    | donePayments =
                        let
                            updateSet =
                                if done then
                                    Set.insert

                                else
                                    Set.remove
                        in
                        model.donePayments |> updateSet paymentId
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


findExistingPaymentWithSameParticipants : Payment -> Int -> List Payment -> Maybe ( Int, Amount )
findExistingPaymentWithSameParticipants payment idx existingPayments =
    case existingPayments of
        [] ->
            Nothing

        existingPayment :: remainingExistingPayments ->
            if existingPayment.payer == payment.payer && existingPayment.receiver == payment.receiver then
                Just ( idx, existingPayment.amount )

            else if existingPayment.payer == payment.receiver && existingPayment.receiver == payment.payer then
                Just ( idx, -existingPayment.amount )

            else
                findExistingPaymentWithSameParticipants payment (idx + 1) remainingExistingPayments


amendPlannedPayments : Set Int -> List Payment -> List Payment -> List Payment
amendPlannedPayments donePayments currentPayments newPayments =
    let
        currentDonePayments =
            currentPayments |> List.filter (\payment -> not <| Set.member payment.id donePayments)

        ( amendedNewPaymentsResult, filteredCurrentPaymentsResult ) =
            List.foldl
                (\newPayment ( amendedNewPayments, filteredCurrentPayments ) ->
                    case findExistingPaymentWithSameParticipants newPayment 0 currentDonePayments of
                        Nothing ->
                            ( newPayment :: amendedNewPayments, filteredCurrentPayments )

                        Just ( index, amount ) ->
                            ( { newPayment | amount = newPayment.amount + amount } :: amendedNewPayments
                            , filteredCurrentPayments |> List.removeAt index
                            )
                )
                ( [], currentPayments )
                newPayments
    in
    amendedNewPaymentsResult ++ filteredCurrentPaymentsResult |> List.reverse


addPayments : List Payment -> Bool -> Int -> Model -> Model
addPayments payments done nextId model =
    { model
        | payments = amendPlannedPayments model.donePayments model.payments payments
        , paymentBalance =
            payments
                |> List.foldl
                    (\payment paymentBalance ->
                        paymentBalance
                            |> updatePaymentBalances payment.payer payment.receiver payment.amount
                    )
                    model.paymentBalance
        , nextId = nextId
        , donePayments =
            if done then
                payments |> List.foldl (.id >> Set.insert) model.donePayments

            else
                model.donePayments
    }


findSuggestedPayment : Dict Int Amount -> Maybe ( ( Int, Amount ), ( Int, Amount ) )
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


autosuggestPayments : Dict Int Amount -> Dict Int (List ( Int, Amount ))
autosuggestPayments totalBalances =
    case totalBalances |> autosuggestPayment of
        Nothing ->
            Dict.empty

        Just ( payerId, receiverId, amount ) ->
            totalBalances
                |> updatePaymentBalances payerId receiverId amount
                |> autosuggestPayments
                |> Dict.update payerId
                    (Maybe.withDefault []
                        >> (::) ( receiverId, amount )
                        >> Just
                    )


autosuggestPayment : Dict Int Amount -> Maybe ( Int, Int, Amount )
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


sumBalances : Int -> Dict Int Amount -> Dict Int Amount -> Amount
sumBalances participantId paymentBalance balance =
    (balance |> lookupBalance participantId) + (paymentBalance |> lookupBalance participantId)


lookupBalance : Int -> Dict Int Amount -> Amount
lookupBalance participantId =
    Dict.get participantId >> Maybe.withDefault 0


suggestPaymentAmount : String -> String -> Dict Int Amount -> Dict Int Amount -> Result ( Maybe Int, Maybe Int ) Amount
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


updatePaymentBalances : Int -> Int -> Amount -> Dict Int Amount -> Dict Int Amount
updatePaymentBalances payerId receiverId amount =
    updatePaymentBalance payerId amount >> updatePaymentBalance receiverId -amount


updatePaymentBalance : Int -> Amount -> Dict Int Amount -> Dict Int Amount
updatePaymentBalance participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)
