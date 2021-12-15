module Payment exposing (..)

import Browser.Dom as Dom
import Dict exposing (Dict)
import Expense exposing (Expense)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Layout exposing (..)
import Maybe.Extra as Maybe
import Participant
import Task
import Util.List as List
import Util.String as String
import Util.Update as Update


createModalId =
    "payment-create"


createModalOpenId =
    createModalId ++ "-open"


type alias Model =
    { create : Maybe CreateModel
    , payments : List Payment
    , paymentBalance : Dict Int Float
    , nextId : Int
    }


type alias Payment =
    { id : Int
    , payer : Int
    , receiver : Int
    , amount : Float
    }


decoder : Decoder Payment
decoder =
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
        (Decode.field "a" Decode.float)
        -- receiver ID
        (Decode.field "r" Decode.int)


encode : Payment -> Value
encode payment =
    [ ( "i", payment.id |> Encode.int )
    , ( "p", payment.payer |> Encode.int )
    , ( "a", payment.amount |> Encode.float )
    , ( "r", payment.receiver |> Encode.int )
    ]
        |> Encode.object


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


create : Int -> CreateModel -> Result String Payment
create id model =
    -- Should probably run values though their validators...
    let
        payerResult =
            case model.payerId |> String.toInt of
                Nothing ->
                    Err <| "unexpected non-integer key '" ++ model.payerId ++ "' of payer"

                Just payerId ->
                    Ok payerId

        receiverResult =
            case model.receiverId |> String.toInt of
                Nothing ->
                    Err <| "unexpected non-integer key '" ++ model.receiverId ++ "' of receiver"

                Just payerId ->
                    Ok payerId

        amountResult =
            case model.amount.value |> String.toFloat of
                Nothing ->
                    Err <| "cannot parse amount '" ++ model.amount.value ++ "' as a (floating point) number"

                Just amount ->
                    Ok amount
    in
    Result.map3
        (\payerId receiverId amount ->
            { id = id
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
    }


type alias CreateModel =
    { payerId : String
    , receiverId : String
    , amount : Validated Field
    , suggestedAmount : Result ( Maybe Int, Maybe Int ) Float
    }


type Msg
    = LoadCreate (List Int)
    | CloseModal
    | CreateEditPayer String
    | CreateEditReceiver String
    | CreateEditAmount String
    | CreateApplySuggestedAmount Float
    | CreateSubmit
    | Delete Int
    | ApplySuggestedPayment Int Int Float
    | ApplyAllSuggestedPayments (Dict Int (List ( Int, Float )))
    | LayoutMsg Layout.Msg
    | DomMsg (Result Dom.Error ())


view : Participant.Model -> Model -> List (Html Msg)
view participantModel model =
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
                        let
                            id =
                                payment.id |> String.fromInt
                        in
                        ( id
                        , Html.tr []
                            [ Html.td [] [ Html.text id ]
                            , Html.td [] [ Html.text (participantModel.idToName |> Participant.lookupName payment.payer) ]
                            , Html.td [] [ Html.text (participantModel.idToName |> Participant.lookupName payment.receiver) ]
                            , Html.td [] [ Html.text (payment.amount |> String.fromAmount) ]
                            , Html.td
                                [ Html.Attributes.align "right", Html.Events.onClick (Delete payment.id) ]
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
        [ Html.Attributes.class "w-100"
        , Html.Attributes.disabled (participantModel.participants |> List.isEmpty)
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
        [ Html.Events.onSubmit CreateSubmit ]
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
                        [ text <| "Suggested amount: " ++ (amount |> String.fromAmount) ]
                    ]
        ]
    , textInput "Amount" model.amount CreateEditAmount
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    modalClosed ModalClosed |> Sub.map LayoutMsg


update : Maybe (Dict Int Float) -> Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update balances msg model =
    case msg of
        LoadCreate participants ->
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
                                                , feedback = Expense.validateAmount amount
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
                    (amount |> String.fromAmount |> CreateEditAmount)
                    (update balances)

        CreateSubmit ->
            let
                id =
                    model.nextId

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
                    ( ( model, False ), Cmd.none )

                Ok payment ->
                    ( ( model |> addPayments [ payment ] (id + 1), True )
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
                        }
                      , True
                      )
                    , createModalOpenId |> Dom.focus |> Task.attempt DomMsg
                    )

        ApplySuggestedPayment payerId receiverId amount ->
            ( ( model
                    |> addPayments
                        [ { id = model.nextId
                          , payer = payerId
                          , receiver = receiverId
                          , amount = amount
                          }
                        ]
                        (model.nextId + 1)
              , True
              )
            , Cmd.none
            )

        ApplyAllSuggestedPayments suggestedPayments ->
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
                            ( [], model.nextId + 1 )
            in
            ( ( model |> addPayments (paymentsReversed |> List.reverse) nextId, True ), Cmd.none )

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


addPayments : List Payment -> Int -> Model -> Model
addPayments payments nextId model =
    { model
        | payments = model.payments ++ payments
        , paymentBalance =
            payments
                |> List.foldl
                    (\payment paymentBalance ->
                        paymentBalance
                            |> updatePaymentBalances payment.payer payment.receiver payment.amount
                    )
                    model.paymentBalance
        , nextId = nextId
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
                |> Dict.update payerId
                    (Maybe.withDefault []
                        >> (::) ( receiverId, amount )
                        >> Just
                    )


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
