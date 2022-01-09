module Expense exposing (..)

import Amount exposing (Amount)
import Browser.Dom as Dom
import Config exposing (Config)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Layout exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Participant exposing (Participant)
import Set exposing (Set)
import Task
import Util.Dict as Dict
import Util.Update as Update


createModalId =
    "expense-create"


createModalOpenId =
    createModalId ++ "-open"


maxDescriptionLength =
    50


type Msg
    = LoadCreate (Maybe Int)
    | CloseModal
    | CreateEditPayer String
    | CreateEditAmount String
    | CreateEditDescription String
    | CreateEditReceiver String Bool
    | CreateSubmit
    | Delete Int
    | LayoutMsg Layout.Msg
    | ParticipantMsg Participant.Msg
    | DomMsg (Result Dom.Error ())


type alias Model =
    { create : Maybe CreateModel
    , expenses : List Expense
    , participant : Participant.Model
    , nextId : Int
    }


{-| Model of the form for creating a new expense.
The strings in this type are really ints but keeping them as strings saves conversions to/from HTML.
-}
type alias CreateModel =
    { payerId : String
    , amount : Validated Field
    , description : Validated Field
    , receivers : Dict String Float -- TODO should just be Set
    , editId : Maybe Int
    }


type alias Expense =
    { id : Int
    , payer : Int
    , amount : Amount
    , description : String
    , receivers : Dict Int Float -- map from participant ID to fractional part
    }


decoder : Decoder Expense
decoder =
    Decode.map5
        Expense
        -- ID
        (Decode.field "i" Decode.int)
        -- payer ID
        (Decode.field "p" Decode.int)
        -- amount
        (Decode.field "a" Decode.int)
        -- description
        (Decode.maybe (Decode.field "d" Decode.string) |> Decode.map (Maybe.withDefault ""))
        -- receivers
        (Decode.field "r"
            (Decode.list Decode.int
                |> Decode.map
                    (List.map (\id -> ( id, 1 )) >> Dict.fromList)
            )
        )


encode : Expense -> Value
encode expense =
    [ Just ( "i", expense.id |> Encode.int )
    , Just ( "p", expense.payer |> Encode.int )
    , Just ( "a", expense.amount |> Amount.encode )
    , if expense.description |> String.isEmpty then
        Nothing

      else
        Just ( "d", expense.description |> Encode.string )
    , Just
        ( "r"
        , expense.receivers
            |> Dict.toList
            |> List.map Tuple.first
            |> Encode.list Encode.int
        )
    ]
        |> Maybe.values
        |> Encode.object


import_ : List Participant -> List Expense -> Model -> Model
import_ participants expenses model =
    { model
        | participant =
            model.participant
                |> Participant.import_ participants
        , expenses = expenses
        , nextId =
            1
                + (expenses
                    |> List.foldl
                        (\expense -> max expense.id)
                        (model.nextId - 1)
                  )
    }


create : Config -> Int -> CreateModel -> Result String Expense
create config id model =
    -- Should probably run values though their validators...
    let
        payerResult =
            case model.payerId |> String.toInt of
                Nothing ->
                    Err <| "unexpected non-integer key '" ++ model.payerId ++ "' of payer"

                Just payerId ->
                    Ok payerId

        amountResult =
            case model.amount.value |> Amount.fromString config.amount of
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
            { id = id
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
    let
        ( participantModel, participantCmd ) =
            Participant.init
    in
    ( { create = Nothing
      , expenses = []
      , participant = participantModel
      , nextId = 1
      }
    , Cmd.batch
        [ participantCmd |> Cmd.map ParticipantMsg
        , Participant.createId |> Dom.focus |> Task.attempt DomMsg
        ]
    )


initCreate : String -> Dict String Float -> CreateModel
initCreate payerId receiverIds =
    { payerId = payerId
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
    , receivers = receiverIds
    , editId = Nothing
    }


view : Config -> Model -> List (Html Msg)
view config model =
    [ viewInstructions
    , Html.table [ class "table" ]
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
                                        -- HACK Using best-guess value for next ID as key in attempt to prevent
                                        -- redundant removal/reinsertion of the cell containing the '+' button
                                        -- (see 'https://github.com/elm/virtual-dom/issues/178').
                                        -- This breaks the focus originally put on the element,
                                        -- but due to the mutation observer also causes the tooltip to be
                                        -- disposed and recreated for no good reason.
                                        [ ( model.participant.nextId |> String.fromInt, Html.td [] [ Html.i [] [ Html.text "None" ] ] ) ]

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
                        let
                            id =
                                expense.id |> String.fromInt
                        in
                        ( id
                        , Html.tr []
                            ([ Html.td [] [ Html.text id ]
                             , Html.td [] [ Html.text (model.participant.idToName |> Participant.lookupName expense.payer) ]
                             , Html.td []
                                [ Html.span
                                    [ data "bs-toggle" "tooltip"
                                    , data "bs-placement" "bottom"
                                    , Html.Attributes.title <| ((expense.amount // Dict.size expense.receivers) |> Amount.toString config.amount) ++ " per participant"
                                    ]
                                    [ Html.text (expense.amount |> Amount.toString config.amount)
                                    ]
                                ]
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
                                        [ Html.Attributes.align "right", Html.Events.onClick (LoadCreate (Just expense.id)) ]
                                        [ Html.a
                                            [ data "bs-toggle" "tooltip"
                                            , data "bs-placement" "left"
                                            , Html.Attributes.class "text-reset"
                                            , Html.Attributes.title "Edit"
                                            , Html.Attributes.attribute "role" "button"
                                            ]
                                            [ Html.i
                                                [ class "bi bi-pencil-square"
                                                , data "bs-toggle" "modal"
                                                , data "bs-target" ("#" ++ createModalId)
                                                ]
                                                []
                                            ]
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


viewInstructions : Html msg
viewInstructions =
    infoBox
        [ Html.p []
            [ Html.text "Start by adding participants for everyone involved using the "
            , Html.i [ class "bi bi-file-plus" ] []
            , Html.text " button in the table's top right corner."
            ]
        ]


viewCreateOpen : Model -> Html Msg
viewCreateOpen model =
    let
        disabled =
            model.participant.participants |> List.isEmpty

        html =
            openModalButton
                createModalOpenId
                createModalId
                "Add expense"
                [ Html.Attributes.class "w-100"
                , Html.Attributes.disabled disabled
                , Html.Events.onClick (LoadCreate Nothing)
                ]
    in
    if disabled then
        Html.div
            [ data "bs-toggle" "tooltip"
            , data "bs-placement" "top"
            , Html.Attributes.title "Add participants to enable expenses"
            , Html.Attributes.tabindex 0
            ]
            [ html ]

    else
        html


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
        [ modal createModalId "Add expense" body disable (model.create |> Maybe.andThen .editId |> Maybe.map Delete) ]


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
        ++ (if Dict.keys model.receivers == [ model.payerId ] then
                [ Html.div
                    [ Html.Attributes.class "alert alert-warning"
                    , Html.Attributes.attribute "role" "alert"
                    ]
                    [ Html.text "The only receiver of the expense is the payer themself. The expense will not effect any balances."
                    ]
                ]

            else
                []
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ modalClosed ModalClosed |> Sub.map LayoutMsg
    , model.participant |> Participant.subscriptions |> Sub.map ParticipantMsg
    ]
        |> Sub.batch


update : Config -> Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update config msg model =
    case msg of
        LoadCreate editId ->
            let
                createModel =
                    case editId of
                        Nothing ->
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

                        Just id ->
                            case model.expenses |> List.find (.id >> (==) id) of
                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "error" "not found..."
                                    in
                                    model.create

                                Just expense ->
                                    let
                                        newCreateModel =
                                            initCreate
                                                (expense.payer |> String.fromInt)
                                                (expense.receivers |> Dict.mapKeys String.fromInt)

                                        descriptionField =
                                            newCreateModel.description

                                        amountField =
                                            newCreateModel.amount
                                    in
                                    Just
                                        { newCreateModel
                                            | description =
                                                { descriptionField | value = expense.description }
                                            , amount = { amountField | value = expense.amount |> Amount.toString config.amount }
                                            , editId = editId
                                        }
            in
            if createModel |> Maybe.isNothing then
                ( ( model, False ), Cmd.none )

            else
                ( ( { model | create = createModel }, False ), Cmd.none )

        CreateSubmit ->
            case model.create of
                Nothing ->
                    let
                        -- TODO Print error on page.
                        _ =
                            Debug.log "error" "no create model found"
                    in
                    ( ( model, False ), Cmd.none )

                Just createModel ->
                    case createModel.editId of
                        Nothing ->
                            let
                                expense =
                                    createModel |> create config model.nextId
                            in
                            case expense of
                                Err error ->
                                    let
                                        -- TODO Print error on page.
                                        _ =
                                            Debug.log "error" error
                                    in
                                    ( ( model, False ), Cmd.none )

                                Ok value ->
                                    ( ( { model
                                            | expenses = model.expenses ++ [ value ]
                                            , nextId = model.nextId + 1
                                        }
                                      , True
                                      )
                                    , Update.delegate CloseModal
                                    )

                        Just editId ->
                            let
                                expense =
                                    createModel |> create config editId
                            in
                            case expense of
                                Err error ->
                                    let
                                        -- TODO Print error on page.
                                        _ =
                                            Debug.log "error" error
                                    in
                                    ( ( model, False ), Cmd.none )

                                Ok value ->
                                    ( ( { model
                                            | expenses = model.expenses |> List.setIf (.id >> (==) editId) value
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
                                                , feedback = validateAmountInput config.amount validateExpenseAmount amount
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
            , Cmd.batch
                [ createModalOpenId |> Dom.focus |> Task.attempt DomMsg
                , Update.delegate CloseModal
                ]
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
                ( ( participantModel, participantModelChanged ), newParticipantCmd ) =
                    Participant.update participantMsg model.participant
            in
            ( ( { model | participant = participantModel }, participantModelChanged )
            , newParticipantCmd |> Cmd.map ParticipantMsg
            )


validateAmountInput : Amount.Config -> (Amount -> Feedback) -> String -> Feedback
validateAmountInput locale validateAmountValue input =
    if input |> String.isEmpty then
        None

    else
        case input |> Amount.fromString locale of
            Nothing ->
                Error "Not a number."

            Just amount ->
                if abs amount > Amount.max then
                    Error "Numerical value is too large."

                else
                    validateAmountValue amount


validateExpenseAmount : Amount -> Feedback
validateExpenseAmount amount =
    if amount < 0 then
        Info "An expense with a negative amount corresponds to an evenly distributed debt or income."

    else
        None


validateDescription : String -> Feedback
validateDescription description =
    if String.length description > maxDescriptionLength then
        Error <| "Longer than " ++ String.fromInt maxDescriptionLength ++ " characters."

    else
        None
