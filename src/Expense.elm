module Expense exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Layout exposing (..)
import Maybe.Extra as Maybe
import Participant exposing (Participant)
import Round
import Set exposing (Set)
import Util.Dict as Dict
import Util.Update as Update


type Msg
    = LoadCreate
    | CreateSubmit
    | CreateEditAmount String
    | CreateEditPayer String
    | CreateEditReceiver String Bool
    | CloseModal
    | LayoutMsg Layout.Msg


type alias Model =
    { create : Maybe CreateModel
    , expenses : List Expense
    }


{-| Model of the form for creating a new expense.
The strings in this type are really ints but keeping them as strings saves conversions to/from HTML.
-}
type alias CreateModel =
    { amount : Validated Field
    , payerId : String
    , receivers : Dict String Float
    }


type alias Expense =
    { payer : Int
    , amount : Float
    , receivers : Dict Int Float -- map from participant ID to fractional part
    }


create : CreateModel -> Result String Expense
create model =
    let
        payerResult =
            model.payerId
                |> String.toInt
                |> Result.fromMaybe ("unexpected non-integer key '" ++ model.payerId ++ "' of payer")

        amountResult =
            model.amount.value
                |> String.toFloat
                |> Result.fromMaybe "cannot parse amount"

        receiverResult =
            model.receivers
                |> Dict.parseKeys
                    (\key ->
                        key
                            |> String.toInt
                            |> Result.fromMaybe ("unexpected non-integer receiver '" ++ key ++ "'")
                    )
    in
    Result.map3
        (\payerId amount receivers ->
            { payer = payerId
            , amount = amount
            , receivers = receivers
            }
        )
        payerResult
        amountResult
        receiverResult


init : Model
init =
    { create = Nothing
    , expenses = []
    }


initCreate : String -> Dict String Float -> CreateModel
initCreate initPayerId initReceiverIds =
    { payerId = initPayerId
    , amount =
        { key = "new-expense-amount"
        , value = ""
        , validationError = Nothing
        }
    , receivers = initReceiverIds
    }


createModalId =
    "add-expense"


view : Participant.Model -> Model -> Html Msg
view participantModel model =
    row1 <|
        Html.form [ Html.Events.onSubmit CreateSubmit ] <|
            [ openModalButton createModalId "Add" LoadCreate
            , let
                ( body, disable ) =
                    case model.create of
                        Nothing ->
                            ( [ Html.text "Loading..." ], True )

                        Just createModel ->
                            ( viewAdd participantModel createModel
                            , String.isEmpty createModel.amount.value
                                || Maybe.isJust createModel.amount.validationError
                            )
              in
              modal createModalId "Add expense" body disable
            ]
                ++ viewExpenses participantModel model.expenses


viewAdd : Participant.Model -> CreateModel -> List (Html Msg)
viewAdd participantModel model =
    let
        participantsFields =
            List.map Participant.toField participantModel.participants
    in
    [ optionsInput "new-expense-payer" "Payer" participantsFields model.payerId CreateEditPayer
    , textInput "Amount" model.amount CreateEditAmount
    , checkboxesInput "Receivers" participantsFields (model.receivers |> Dict.keys |> Set.fromList) CreateEditReceiver
    ]


viewExpenses : Participant.Model -> List Expense -> List (Html Msg)
viewExpenses participantModel expenseModels =
    [ Html.table [ class "table" ]
        [ Html.thead []
            [ Html.tr [] <|
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ]
                ]
                    ++ List.map
                        (.name >> Html.text >> List.singleton >> Html.th [ Html.Attributes.scope "col" ])
                        participantModel.participants
            ]
        , Html.tbody [] <|
            List.map
                (\expense ->
                    Html.tr []
                        ([ Html.td [] [ Participant.lookupName expense.payer participantModel.names |> Html.text ]
                         , Html.td [] [ expense.amount |> Round.round 2 |> Html.text ]
                         ]
                            ++ List.map
                                (\participant ->
                                    Html.td []
                                        (if Dict.member participant.id expense.receivers then
                                            [ Html.text "✓" ]

                                         else
                                            []
                                        )
                                )
                                participantModel.participants
                        )
                )
                expenseModels
        ]
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    modalClosed ModalClosed |> Sub.map LayoutMsg


update : List Participant -> Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update participants msg model =
    case msg of
        LoadCreate ->
            ( ( { model
                    | create =
                        participants
                            |> List.head
                            |> Maybe.map
                                (\firstParticipant ->
                                    initCreate
                                        (firstParticipant.id |> String.fromInt)
                                        (participants |> List.map (.id >> String.fromInt) |> List.map (\key -> ( key, 1.0 )) |> Dict.fromList)
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateSubmit ->
            let
                expense =
                    model.create
                        |> Result.fromMaybe "no create model found"
                        |> Result.andThen create
            in
            case expense of
                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( ( model, False ), Cmd.none )

                Ok value ->
                    ( ( { model
                            | expenses =
                                model.expenses ++ [ value ]
                        }
                      , True
                      )
                    , Update.delegate CloseModal
                    )

        CloseModal ->
            ( ( model, False ), closeModal createModalId )

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
                                                , validationError = validateAmount amount
                                            }
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateEditPayer payer ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map (\createModel -> { createModel | payerId = payer })
                }
              , False
              )
            , Cmd.none
            )

        CreateEditReceiver receiverKey checked ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel ->
                                    { createModel
                                        | receivers =
                                            if checked then
                                                Dict.insert receiverKey 1.0 createModel.receivers

                                            else
                                                Dict.remove receiverKey createModel.receivers
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        LayoutMsg layoutMsg ->
            case layoutMsg of
                ModalClosed modalId ->
                    if modalId == createModalId then
                        ( ( { model | create = Nothing }, False ), Cmd.none )

                    else
                        ( ( model, False ), Cmd.none )


validateAmount : String -> Maybe String
validateAmount amount =
    if String.isEmpty amount then
        Nothing

    else
        case String.toFloat amount of
            Nothing ->
                Just "Not a number."

            Just _ ->
                Nothing
