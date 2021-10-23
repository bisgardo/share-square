module Expense exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Html.Keyed
import Layout exposing (..)
import Maybe.Extra as Maybe
import Participant exposing (Participant)
import Round
import Set exposing (Set)
import Util.Dict as Dict
import Util.Update as Update


createModalId =
    "expense-create"


maxDescriptionLength =
    50


type Msg
    = LoadCreate
    | CreateSubmit
    | CloseModal
    | CreateEditAmount String
    | CreateEditPayer String
    | CreateEditReceiver String Bool
    | CreateEditDescription String
    | LayoutMsg Layout.Msg
    | ParticipantMsg Participant.Msg


type alias Model =
    { create : Maybe CreateModel
    , expenses : List Expense
    , participant : Participant.Model
    , nextExpenseId : Int
    }


{-| Model of the form for creating a new expense.
The strings in this type are really ints but keeping them as strings saves conversions to/from HTML.
-}
type alias CreateModel =
    { amount : Validated Field
    , payerId : String
    , receivers : Dict String Float
    , description : Validated Field
    }


type alias Expense =
    { id : String
    , payer : Int
    , amount : Float
    , receivers : Dict Int Float -- map from participant ID to fractional part
    , description : String
    }


create : Int -> CreateModel -> Result String Expense
create id model =
    -- Should probably run values though their validators...
    let
        payerResult =
            model.payerId
                |> String.toInt
                |> Result.fromMaybe ("unexpected non-integer key '" ++ model.payerId ++ "' of payer")

        amountResult =
            model.amount.value
                |> String.toFloat
                |> Result.fromMaybe ("cannot parse amount '" ++ model.amount.value ++ "' as a (floating point) number")

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
            { id = id |> String.fromInt
            , payer = payerId
            , amount = amount
            , receivers = receivers
            , description = model.description.value
            }
        )
        payerResult
        amountResult
        receiverResult


init : Model
init =
    { create = Nothing
    , expenses = []
    , participant = Participant.init
    , nextExpenseId = 0
    }


initCreate : String -> Dict String Float -> CreateModel
initCreate initPayerId initReceiverIds =
    { payerId = initPayerId
    , amount =
        { key = "expense-create-amount"
        , value = ""
        , validationError = Nothing
        }
    , receivers = initReceiverIds
    , description =
        { key = "expense-create-description"
        , value = ""
        , validationError = Nothing
        }
    }


view : Model -> Html Msg
view model =
    row <|
        [ Html.table [ class "table" ]
            [ Html.thead []
                -- Use keyed HTML to avoid replacement of "+" button as that breaks the tooltip.
                [ Html.Keyed.node "tr" [] <|
                    [ ( "payer", Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ] )
                    , ( "amount", Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ] )
                    ]
                        ++ (model.participant.participants
                                |> List.map
                                    (\participant ->
                                        let
                                            name =
                                                participant.name
                                        in
                                        ( name
                                        , name
                                            |> Html.text
                                            |> List.singleton
                                            |> Html.th [ Html.Attributes.scope "col" ]
                                        )
                                    )
                           )
                        ++ [ ( Participant.createModalId
                               -- TODO Figure out how to align properly.
                             , Html.th
                                []
                                [ Html.div [ Html.Attributes.style "overflow" "auto" ] [ Html.div [ class "float-end" ] [ Participant.viewCreateOpen |> Html.map ParticipantMsg ] ]
                                , Html.div [] [ Html.text "Description" ]
                                ]
                             )
                           ]
                ]
            , Html.Keyed.node "tbody" [] <|
                (model.expenses
                    |> List.map
                        (\expense ->
                            ( expense.id
                            , Html.tr []
                                ([ Html.td [] [ Participant.lookupName expense.payer model.participant.names |> Html.text ]
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
                                        model.participant.participants
                                    ++ [ Html.td [] [ expense.description |> Html.text ] ]
                                )
                            )
                        )
                )
            ]
        , viewCreateOpen model
        , Participant.viewCreateModal model.participant |> Html.map ParticipantMsg
        , viewCreateModal model
        ]


viewCreateOpen : Model -> Html Msg
viewCreateOpen model =
    openModalButton
        Participant.createId
        createModalId
        "Add"
        [ List.isEmpty model.participant.participants
            |> Html.Attributes.disabled
        , Html.Events.onClick LoadCreate
        ]


viewCreateModal : Model -> Html Msg
viewCreateModal model =
    let
        ( body, disable ) =
            case model.create of
                Nothing ->
                    ( [ Html.text "Loading..." ], True )

                Just createModel ->
                    ( viewAdd model.participant createModel
                    , String.isEmpty createModel.amount.value
                        || Maybe.isJust createModel.amount.validationError
                        || Maybe.isJust createModel.description.validationError
                    )
    in
    Html.form
        [ Html.Events.onSubmit CreateSubmit ]
        [ modal createModalId "Add expense" body disable ]


viewAdd : Participant.Model -> CreateModel -> List (Html Msg)
viewAdd participantModel model =
    let
        participantsFields =
            List.map Participant.toField participantModel.participants
    in
    [ optionsInput "new-expense-payer" "Payer" participantsFields model.payerId CreateEditPayer
    , textInput "Amount" model.amount CreateEditAmount
    , checkboxesInput "Receivers" participantsFields (model.receivers |> Dict.keys |> Set.fromList) CreateEditReceiver
    , textInput "Description" model.description CreateEditDescription
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    modalClosed ModalClosed |> Sub.map LayoutMsg


update : Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update msg model =
    case msg of
        LoadCreate ->
            ( ( { model
                    | create =
                        model.participant.participants
                            |> List.head
                            |> Maybe.map
                                (\firstParticipant ->
                                    initCreate
                                        (firstParticipant.id |> String.fromInt)
                                        (model.participant.participants
                                            |> List.map (.id >> String.fromInt)
                                            |> List.map (\key -> ( key, 1.0 ))
                                            |> Dict.fromList
                                        )
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateSubmit ->
            let
                id =
                    model.nextExpenseId

                expense =
                    model.create
                        |> Result.fromMaybe "no create model found"
                        |> Result.andThen (create id)
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
                            , nextExpenseId = id + 1
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

        CreateEditDescription description ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel ->
                                    let
                                        descriptionField =
                                            createModel.description
                                    in
                                    { createModel
                                        | description =
                                            { descriptionField
                                                | value = description
                                                , validationError = validateDescription description
                                            }
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

        ParticipantMsg participantMsg ->
            let
                ( newParticipantModel, newParticipantCmd ) =
                    Participant.update participantMsg model.participant
            in
            ( ( { model | participant = newParticipantModel }, False )
            , Cmd.map ParticipantMsg newParticipantCmd
            )


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


validateDescription : String -> Maybe String
validateDescription description =
    if String.length description > maxDescriptionLength then
        Just <| "Longer than " ++ String.fromInt maxDescriptionLength ++ " characters."

    else
        Nothing
