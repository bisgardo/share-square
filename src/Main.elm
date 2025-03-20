module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Config exposing (Config)
import Dict
import Domain.Expense as Expense exposing (Expense)
import Domain.Participant as Participant exposing (Participant)
import Domain.Payment as Payment exposing (Payment)
import Domain.Settlement as Settlement
import Expense
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Layout exposing (..)
import LocalStorage exposing (Revision)
import Maybe.Extra as Maybe
import Participant
import Payment
import Settlement
import Storage
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query
import Util.Update as Update


schemaVersion =
    "1"


schemaVersionSplitter =
    "|"


storageDataKey =
    "data"


storageUrlKey =
    "storage"


storageUrlNone =
    "none"


storageUrlLocal =
    "local"


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , key : Browser.Navigation.Key
    , config : Config
    , expense : Expense.Model
    , computation : Settlement.Model
    , storageMode : Storage.Mode
    , storageRevision : Revision
    , errors : List Error
    }


type Error
    = StorageError StorageError


type StorageError
    = ReadError StorageReadError
    | WriteError StorageWriteError


type StorageReadError
    = InvalidFormat String
    | UnknownSchemaVersion String
    | ParseError String


type StorageWriteError
    = RevisionMismatch Revision Revision


type alias StorageConfig =
    { decimalPlaces : Int
    }


type alias StorageValues =
    { participants : List Participant
    , expenses : List Expense
    , payments : List Payment
    , config : StorageConfig
    , settledBy : Settlement.SettledBy
    }


modeParser : Parser (Maybe Storage.Mode -> a) a
modeParser =
    (Url.Parser.top <?> Url.Parser.Query.string storageUrlKey)
        |> Url.Parser.map
            (Maybe.andThen
                (\value ->
                    if value == storageUrlNone then
                        Just Storage.None

                    else if value == storageUrlLocal then
                        Just Storage.Local

                    else
                        Nothing
                )
            )


storageConfigDecoder : Decoder StorageConfig
storageConfigDecoder =
    Decode.map
        StorageConfig
        -- decimal places (in amount values)
        (Decode.field "p" <| Decode.int)


encodeStorageConfig : StorageConfig -> Value
encodeStorageConfig values =
    [ ( "p", values.decimalPlaces |> Encode.int ) ]
        |> Encode.object


storageValuesDecoder : Decoder StorageValues
storageValuesDecoder =
    Decode.map4
        (\participantsAndSettledBy expenses payments config ->
            let
                participants =
                    participantsAndSettledBy |> List.map Tuple.first

                settledBy =
                    participantsAndSettledBy
                        |> List.foldl
                            (\( participant, participantSettledBy ) ->
                                Dict.update participant.id (always participantSettledBy)
                            )
                            Dict.empty
            in
            StorageValues participants expenses payments config settledBy
        )
        -- participants
        (Decode.field "p" <| Decode.list Participant.decoder)
        -- expenses
        (Decode.field "e" <| Decode.list Expense.decoder)
        -- payments
        (Decode.field "y" <| Decode.list Payment.decoder)
        -- config
        (Decode.field "c" <| storageConfigDecoder)


withSettledBy : Settlement.SettledBy -> List Participant -> List ( Participant, Maybe Participant.Id )
withSettledBy settledBy participants =
    participants
        |> List.map
            (\participant ->
                ( participant, settledBy |> Dict.get participant.id )
            )


encodeStorageValues : StorageValues -> Value
encodeStorageValues values =
    [ ( "p", values.participants |> withSettledBy values.settledBy |> Encode.list Participant.encode )
    , ( "e", values.expenses |> Encode.list Expense.encode )
    , ( "y", values.payments |> Encode.list Payment.encode )
    , ( "c", values.config |> encodeStorageConfig )
    ]
        |> Encode.object


type SyncDirection
    = FromStorage
    | ToStorage


type Msg
    = ExpenseMsg Expense.Msg
    | ComputationMsg Settlement.Msg
    | SetMode Storage.Mode SyncDirection
    | StorageValuesLoaded (Maybe ( Revision, Result Error StorageValues ))
    | StorageValuesStored (Result Revision Revision)
    | SyncStorage SyncDirection
    | UrlRequested UrlRequest
    | UrlChanged Url


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        storageMode =
            Url.Parser.parse modeParser url
                |> Maybe.join
                -- Missing or invalid value for "storage" parameter will default to "local".
                |> Maybe.withDefault Storage.Local

        ( expenseModel, expenseCmd ) =
            Expense.init

        ( computationModel, computationCmd ) =
            Settlement.init
    in
    ( { environment = flags.environment
      , key = key
      , config = Config.default
      , expense = expenseModel
      , computation = computationModel
      , storageMode = storageMode
      , storageRevision = 0
      , errors = []
      }
    , Cmd.batch
        [ expenseCmd |> Cmd.map ExpenseMsg
        , computationCmd |> Cmd.map ComputationMsg
        ]
    )
        |> Update.chain update (SetMode storageMode FromStorage)


subscriptions : Model -> Sub Msg
subscriptions model =
    [ model.expense
        |> Expense.subscriptions
        |> Sub.map ExpenseMsg
    , model.computation
        |> Settlement.subscriptions
        |> Sub.map ComputationMsg
    , Storage.valueLoaded
        (\result ->
            case result of
                Nothing ->
                    StorageValuesLoaded Nothing

                Just ( _, revision, values ) ->
                    StorageValuesLoaded <|
                        Just
                            ( revision
                            , (case extractVersion values of
                                Just ( version, data ) ->
                                    if version == schemaVersion then
                                        data
                                            |> Decode.decodeString storageValuesDecoder
                                            |> Result.mapError (ParseError << Decode.errorToString)

                                    else
                                        Err <| UnknownSchemaVersion version

                                _ ->
                                    Err <| InvalidFormat values
                              )
                                |> Result.mapError (StorageError << ReadError)
                            )
        )
    , Storage.valueStored (\( _, result ) -> StorageValuesStored result)
    ]
        |> Sub.batch


extractVersion : String -> Maybe ( String, String )
extractVersion string =
    string
        |> String.indexes schemaVersionSplitter
        |> List.head
        |> Maybe.map
            (\splitterIndex ->
                ( string |> String.left splitterIndex
                , string |> String.dropLeft (splitterIndex + 1)
                )
            )


tabIds =
    { expenses = "expenses"
    , settlement = "settlement"
    }


settlementToggleId =
    "settlement-toggle"


view : Model -> Browser.Document Msg
view model =
    { title = "Share 'n Square"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    container <|
        [ Html.div
            [ Html.Attributes.class "mb-4" ]
            [ Html.h1 [ Html.Attributes.class "d-inline" ] [ Html.text "Share 'n square" ]
            , Html.p [ Html.Attributes.class "lead d-inline ms-2" ] [ Html.text "Expense calculator" ]
            , Html.div [ Html.Attributes.class "float-end" ] <| viewStorageModeSelector model.storageMode
            ]
        ]
            ++ (model.errors
                    |> List.map
                        (\error ->
                            Html.div [ Html.Attributes.class "alert alert-danger", Html.Attributes.attribute "role" "alert" ]
                                [ Html.text <|
                                    case error of
                                        StorageError storageError ->
                                            "Storage error: "
                                                ++ (case storageError of
                                                        ReadError storageReadError ->
                                                            "Cannot read state: "
                                                                ++ (case storageReadError of
                                                                        InvalidFormat message ->
                                                                            "Invalid data: " ++ message

                                                                        UnknownSchemaVersion version ->
                                                                            "Unknown schema version \"" ++ version ++ "\""

                                                                        ParseError message ->
                                                                            "Parse error: " ++ message
                                                                   )

                                                        WriteError storageWriteError ->
                                                            "Cannot write state: "
                                                                ++ (case storageWriteError of
                                                                        RevisionMismatch storedRevision documentRevision ->
                                                                            "Revision mismatch (stored state has revision "
                                                                                ++ (storedRevision |> String.fromInt)
                                                                                ++ " but the app state has revision "
                                                                                ++ (documentRevision |> String.fromInt)
                                                                                ++ "). Reload the page to recover."
                                                                   )
                                                   )
                                ]
                        )
               )
            ++ viewContent model


viewStorageModeSelector : Storage.Mode -> List (Html Msg)
viewStorageModeSelector mode =
    [ Html.div [ Html.Attributes.class "form-check form-check-inline" ]
        [ Html.label
            [ Html.Attributes.class "form-check-label"
            , Html.Attributes.for "storage-mode-none"
            , data "bs-toggle" "tooltip"
            , data "bs-placement" "bottom"
            , Html.Attributes.title "Refreshing the browser window will clear all data."
            ]
            [ Html.text "No storage"
            , Html.input
                [ Html.Attributes.class "form-check-input"
                , Html.Attributes.id "storage-mode-none"
                , Html.Attributes.name "storage-mode"
                , Html.Attributes.type_ "radio"
                , Html.Attributes.checked (mode == Storage.None)
                , Html.Events.onCheck (SetMode Storage.None ToStorage |> always)
                ]
                []
            ]
        ]
    , Html.div [ Html.Attributes.class "form-check form-check-inline" ]
        [ Html.label
            [ Html.Attributes.class "form-check-label"
            , Html.Attributes.for "storage-mode-localstorage"
            ]
            [ Html.text "Use local storage"
            , Html.input
                [ Html.Attributes.class "form-check-input"
                , Html.Attributes.id "storage-mode-localstorage"
                , Html.Attributes.name "storage-mode"
                , Html.Attributes.type_ "radio"
                , Html.Attributes.checked (mode == Storage.Local)
                , Html.Events.onCheck (SetMode Storage.Local ToStorage |> always)
                ]
                []
            ]
        ]
    ]


viewContent : Model -> List (Html Msg)
viewContent model =
    let
        disableSettlementTab =
            model.expense.expenses |> List.isEmpty
    in
    [ viewInstructions
    , Html.ul [ Html.Attributes.class "nav nav-tabs mb-2" ]
        [ Html.li [ Html.Attributes.class "nav-item" ]
            [ Html.button
                [ Html.Attributes.type_ "button"
                , data "bs-toggle" "tab"
                , data "bs-target" ("#" ++ tabIds.expenses)
                , Html.Attributes.class "nav-link active"
                ]
                [ Html.text "Expenses" ]
            ]
        , Html.li [ Html.Attributes.class "nav-item" ]
            [ if disableSettlementTab then
                Html.div
                    [ data "bs-toggle" "tooltip"
                    , data "bs-placement" "right"
                    , Html.Attributes.title "Add expenses to enable settlement"
                    , Html.Attributes.tabindex 0
                    ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Attributes.class "nav-link"
                        , Html.Attributes.class "disabled"
                        , Html.Attributes.disabled True
                        ]
                        [ Html.text "Settlement" ]
                    ]

              else
                Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.id settlementToggleId
                    , data "bs-toggle" "tab"
                    , data "bs-target" ("#" ++ tabIds.settlement)
                    , Html.Attributes.class "nav-link"
                    , Settlement.Enable
                        (model.expense.participant.participants |> Dict.values)
                        model.expense.expenses
                        |> ComputationMsg
                        |> Html.Events.onClick
                    ]
                    [ Html.text "Settlement" ]
            ]
        ]
    , Html.div [ Html.Attributes.class "tab-content" ]
        [ Html.div
            [ Html.Attributes.id tabIds.expenses
            , Html.Attributes.class "tab-pane fade active show"
            ]
            (viewExpenses model
                ++ (if disableSettlementTab then
                        []

                    else
                        [ Html.p [] []
                        , Html.p []
                            [ Html.label [ Html.Attributes.for settlementToggleId, Html.Attributes.class "float-end" ]
                                [ Html.span
                                    [ Html.Attributes.class "btn btn-outline-secondary" ]
                                    [ Html.text "Go to Settlement"
                                    , Html.i [ Html.Attributes.class "bi bi-caret-right" ] []
                                    ]
                                ]
                            ]
                        ]
                   )
            )
        , Html.div
            [ Html.Attributes.id tabIds.settlement
            , Html.Attributes.class "tab-pane fade"
            ]
            (viewComputation model)
        ]
    ]


viewInstructions : Html msg
viewInstructions =
    Html.p [] [ Html.em [] [ Html.text "Square shared expenses independently from currency or payment methods" ] ]


viewExpenses : Model -> List (Html Msg)
viewExpenses model =
    model.expense
        |> Expense.view model.config
        |> List.map (Html.map ExpenseMsg)


viewComputation : Model -> List (Html Msg)
viewComputation model =
    model.computation
        |> Settlement.view model.config model.expense.participant
        |> Html.map ComputationMsg
        |> List.singleton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpenseMsg expenseMsg ->
            let
                ( ( expenseModel, expenseModelChanged ), expensesCmd ) =
                    model.expense |> Expense.update model.config expenseMsg

                ( ( computationModel, computationModelChanged ), computationCmd ) =
                    if expenseModelChanged then
                        model.computation
                            |> Settlement.update model.config model.expense.participant Settlement.Disable

                    else
                        ( ( model.computation, False ), Cmd.none )
            in
            ( { model
                | expense = expenseModel
                , computation = computationModel
              }
            , Cmd.batch
                [ if expenseModelChanged || computationModelChanged then
                    Update.delegate (SyncStorage ToStorage)

                  else
                    Cmd.none
                , expensesCmd |> Cmd.map ExpenseMsg
                , computationCmd |> Cmd.map ComputationMsg
                ]
            )

        ComputationMsg computationMsg ->
            let
                ( ( computationModel, modelChanged ), computationCmd ) =
                    model.computation
                        |> Settlement.update model.config model.expense.participant computationMsg
            in
            ( { model | computation = computationModel }
            , Cmd.batch
                [ if modelChanged then
                    Update.delegate (SyncStorage ToStorage)

                  else
                    Cmd.none
                , computationCmd |> Cmd.map ComputationMsg
                ]
            )

        SetMode mode syncDirection ->
            case mode of
                Storage.None ->
                    ( { model | storageMode = Storage.None, storageRevision = 0 }
                    , Cmd.batch
                        [ Storage.deleteValue storageDataKey model.storageMode
                        , [ Url.Builder.string storageUrlKey storageUrlNone ]
                            |> Url.Builder.toQuery
                            |> Browser.Navigation.replaceUrl model.key
                        ]
                    )

                Storage.Local ->
                    ( { model | storageMode = Storage.Local }
                    , Cmd.batch
                        [ [ Url.Builder.string storageUrlKey storageUrlLocal ]
                            |> Url.Builder.toQuery
                            |> Browser.Navigation.replaceUrl model.key
                        ]
                    )
                        |> Update.chain update (SyncStorage syncDirection)

        StorageValuesLoaded result ->
            case result of
                Nothing ->
                    ( model, Cmd.none )

                Just ( revision, loadedValues ) ->
                    case loadedValues of
                        Err error ->
                            ( { model | errors = model.errors ++ [ error ] }, Cmd.none )

                        Ok values ->
                            ( model |> import_ revision values, Cmd.none )

        StorageValuesStored result ->
            case result of
                Err storedRevision ->
                    ( { model
                        | errors =
                            model.errors
                                ++ [ StorageError <|
                                        WriteError <|
                                            RevisionMismatch storedRevision model.storageRevision
                                   ]
                      }
                    , Cmd.none
                    )

                Ok revision ->
                    ( { model | storageRevision = revision }, Cmd.none )

        SyncStorage direction ->
            ( model
            , case direction of
                FromStorage ->
                    model.storageMode
                        |> Storage.loadValue storageDataKey

                ToStorage ->
                    model.storageMode
                        |> Storage.storeValue storageDataKey
                            model.storageRevision
                            (schemaVersion
                                ++ schemaVersionSplitter
                                ++ (model
                                        |> export
                                        |> encodeStorageValues
                                        |> Encode.encode 0
                                   )
                            )
            )

        UrlRequested _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )


import_ : Revision -> StorageValues -> Model -> Model
import_ revision values model =
    { model
        | expense =
            model.expense
                |> Expense.import_ values.participants values.expenses
        , computation =
            model.computation
                |> Settlement.import_ values.payments values.settledBy
        , storageRevision = revision
        , config =
            let
                config =
                    model.config
            in
            { config
                | amount =
                    { decimalPlaces = values.config.decimalPlaces
                    , decimalSeparator = Config.defaultDecimalSeparator
                    }
            }
    }


export : Model -> StorageValues
export model =
    { participants =
        model.expense.participant.participants
            |> Dict.values
            |> List.sortBy .id
    , expenses = model.expense.expenses
    , payments = model.computation.payment.payments
    , config = { decimalPlaces = model.config.amount.decimalPlaces }
    , settledBy = model.computation.settledBy
    }
