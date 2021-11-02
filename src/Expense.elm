module Expense exposing (..)

import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Html.Keyed
import Layout exposing (..)
import Participant exposing (Participant)
import Set exposing (Set)
import Task
import Util.Dict as Dict
import Util.String as String
import Util.Update as Update


createModalId =
    "expense-create"


createModalOpenId =
    createModalId ++ "-open"


maxDescriptionLength =
    50


type Msg
    = LoadCreate
    | CreateSubmit
    | CloseModal
    | CreateEditPayer String
    | CreateEditAmount String
    | CreateEditDescription String
    | CreateEditReceiver String Bool
    | Delete String
    | LayoutMsg Layout.Msg
    | ParticipantMsg Participant.Msg
    | DomMsg (Result Dom.Error ())


type alias Model =
    { create : Maybe CreateModel
    , expenses : List Expense
    , participant : Participant.Model -- TODO move back out to module Main
    , nextExpenseId : Int
    }


{-| Model of the form for creating a new expense.
The strings in this type are really ints but keeping them as strings saves conversions to/from HTML.
-}
type alias CreateModel =
    { payerId : String
    , amount : Validated Field
    , description : Validated Field
    , receivers : Dict String Float
    }


type alias Expense =
    { id : String
    , payer : Int
    , amount : Float
    , description : String
    , receivers : Dict Int Float -- map from participant ID to fractional part
    }


create : Int -> CreateModel -> Result String Expense
create id model =
    -- Should probably run values though their validators...
    let
        payerResult =
            case model.payerId |> String.toInt of
                Nothing ->
                    Err <| "unexpected non-integer key '" ++ model.payerId ++ "' of payer"

                Just payerId ->
                    Ok payerId

        amountResult =
            case model.amount.value |> String.toFloat of
                Nothing ->
                    Err <| "cannot parse amount '" ++ model.amount.value ++ "' as a (floating point) number"

                Just amount ->
                    Ok amount

        receiverResult =
            model.receivers
                |> Dict.parseKeys
                    (\key ->
                        case key |> String.toInt of
                            Nothing ->
                                Err <| "unexpected non-integer receiver '" ++ key ++ "'"

                            Just receiverId ->
                                Ok receiverId
                    )
    in
    Result.map3
        (\payerId amount receivers ->
            { id = id |> String.fromInt
            , payer = payerId
            , amount = amount
            , description = model.description.value |> String.trim
            , receivers = receivers
            }
        )
        payerResult
        amountResult
        receiverResult


init : ( Model, Cmd Msg )
init =
    ( { create = Nothing
      , expenses = []
      , participant = Participant.init
      , nextExpenseId = 1
      }
    , Dom.focus Participant.createId |> Task.attempt DomMsg
    )


initCreate : String -> Dict String Float -> CreateModel
initCreate initPayerId initReceiverIds =
    { payerId = initPayerId
    , amount =
        { key = "expense-create-amount"
        , value = ""
        , feedback = None
        }
    , description =
        { key = "expense-create-description"
        , value = ""
        , feedback = None
        }
    , receivers = initReceiverIds
    }


view : Model -> List (Html Msg)
view model =
    [ Html.table [ class "table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ Html.Attributes.scope "col" ] [ Html.text "#" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ]
                , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Description" ]
                , Html.th
                    [ Html.Attributes.scope "col"
                    , Html.Attributes.colspan (model.participant.participants |> List.length |> max 1)
                    ]
                    [ Html.text "Participants" ]
                , Html.td [] []
                ]

            -- Must use keyed HTML to avoid replacement of "+" button as that breaks the tooltip.
            , Html.Keyed.node "tr" [] <|
                [ ( "id", Html.td [] [] )
                , ( "payer", Html.td [] [] )
                , ( "amount", Html.td [] [] )
                , ( "description", Html.td [] [] )
                ]
                    ++ (model.participant.participants
                            |> List.map
                                (\participant ->
                                    ( participant.id |> String.fromInt
                                    , Html.td [] [ Html.text participant.name ]
                                    )
                                )
                            |> (\htmls ->
                                    -- Ensure that there is at least 1 cell.
                                    if htmls |> List.isEmpty then
                                        [ ( "empty", Html.td [] [ Html.i [] [ Html.text "None" ] ] ) ]

                                    else
                                        htmls
                               )
                       )
                    ++ [ ( "participant-create"
                         , Html.td [ Html.Attributes.align "right" ]
                            [ Participant.viewCreateOpen |> Html.map ParticipantMsg ]
                         )
                       ]
            ]
        , Html.Keyed.node "tbody"
            []
            (model.expenses
                |> List.map
                    (\expense ->
                        ( expense.id
                        , Html.tr []
                            ([ Html.td [] [ Html.text expense.id ]
                             , Html.td [] [ Html.text (model.participant.idToName |> Participant.lookupName expense.payer) ]
                             , Html.td [] [ Html.text (expense.amount |> String.fromAmount) ]
                             , Html.td [] [ Html.text expense.description ]
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
                                ++ [ Html.td
                                        [ Html.Attributes.align "right", Html.Events.onClick (Delete expense.id) ]
                                        [ Html.a
                                            [ data "bs-toggle" "tooltip"
                                            , data "bs-placement" "left"
                                            , Html.Attributes.title "Delete"
                                            , Html.Attributes.attribute "role" "button"
                                            ]
                                            [ Html.i [ class "bi bi-trash" ] [] ]
                                        ]
                                   ]
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
        createModalOpenId
        createModalId
        "Add expense"
        [ Html.Attributes.disabled (model.participant.participants |> List.isEmpty)
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
                    , (createModel.amount.value |> String.isEmpty)
                        || List.any isInvalid [ createModel.amount, createModel.description ]
                    )
    in
    Html.form
        [ Html.Events.onSubmit CreateSubmit ]
        [ modal createModalId "Add expense" body disable ]


viewAdd : Participant.Model -> CreateModel -> List (Html Msg)
viewAdd participantModel model =
    let
        participantsFields =
            participantModel.participants
                |> List.map Participant.toField
    in
    [ optionsInput "new-expense-payer" "Payer" { fields = participantsFields, feedback = None } model.payerId CreateEditPayer
    , textInput "Amount" model.amount CreateEditAmount
    , textInput "Description" model.description CreateEditDescription
    , checkboxesInput "Receivers" participantsFields (model.receivers |> Dict.keys |> Set.fromList) CreateEditReceiver
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
                    -- TODO Print error on page.
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

        CreateEditPayer payer ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel ->
                                    { createModel | payerId = payer }
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
                                                , feedback = validateAmount amount
                                            }
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
                                                , feedback = validateDescription description
                                            }
                                    }
                                )
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
                                                createModel.receivers |> Dict.insert receiverKey 1.0

                                            else
                                                createModel.receivers |> Dict.remove receiverKey
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        Delete expenseId ->
            ( ( { model
                    | expenses =
                        model.expenses |> List.filter (.id >> (/=) expenseId)
                }
              , True
              )
            , Dom.focus createModalOpenId |> Task.attempt DomMsg
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

        ParticipantMsg participantMsg ->
            let
                ( newParticipantModel, newParticipantCmd ) =
                    Participant.update participantMsg model.participant
            in
            ( ( { model | participant = newParticipantModel }, False )
            , Cmd.map ParticipantMsg newParticipantCmd
            )


validateAmount : String -> Feedback
validateAmount amount =
    if amount |> String.isEmpty then
        None

    else
        case amount |> String.toFloat of
            Nothing ->
                Error "Not a number."

            Just _ ->
                None


validateDescription : String -> Feedback
validateDescription description =
    if String.length description > maxDescriptionLength then
        Error <| "Longer than " ++ String.fromInt maxDescriptionLength ++ " characters."

    else
        None
