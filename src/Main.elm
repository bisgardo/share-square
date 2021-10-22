module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Computation
import Expense
import Html exposing (Html)
import Layout exposing (..)
import Participant
import Task
import Util.List as List


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , participant : Participant.Model
    , expense : Expense.Model
    , computation : Computation.Model
    }


type Msg
    = ParticipantMsg Participant.Msg
    | ExpenseMsg Expense.Msg
    | ComputationMsg Computation.Msg
    | DomMsg (Result Dom.Error ())


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { environment = flags.environment
      , participant = Participant.init
      , expense = Expense.init
      , computation = Computation.init
      }
    , Dom.focus Participant.createId |> Task.attempt DomMsg
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.expense
        |> Expense.subscriptions
        |> Sub.map ExpenseMsg


view : Model -> Browser.Document Msg
view model =
    { title = "Share 'n square"
    , body = [ model |> viewBody ]
    }


viewBody : Model -> Html Msg
viewBody model =
    container <|
        List.concat
            [ viewParticipants model
            , viewExpenses model
            , viewComputation model
            ]


viewParticipants : Model -> List (Html Msg)
viewParticipants model =
    [ Html.h1 [] [ Html.text "Participants" ]
    , model.participant
        |> Participant.view
        |> Html.map ParticipantMsg
    ]


viewExpenses : Model -> List (Html Msg)
viewExpenses model =
    List.ifNonEmpty model.participant.participants <|
        [ Html.h1 [] [ Html.text "Expenses" ]
        , model.expense
            |> Expense.view model.participant
            |> Html.map ExpenseMsg
        ]


viewComputation : Model -> List (Html Msg)
viewComputation model =
    List.ifNonEmpty model.expense.expenses
        [ model.computation
            |> Computation.view model.participant.names
            |> Html.map ComputationMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParticipantMsg participantMsg ->
            let
                ( newParticipantModel, newParticipantCmd ) =
                    Participant.update participantMsg model.participant
            in
            ( { model | participant = newParticipantModel }
            , Cmd.map ParticipantMsg newParticipantCmd
            )

        ExpenseMsg expenseMsg ->
            let
                ( ( newExpenseModel, recompute ), newExpensesCmd ) =
                    Expense.update model.participant.participants expenseMsg model.expense

                ( newComputationModel, newComputationCmd ) =
                    if recompute then
                        Computation.Recompute model.participant.participants newExpenseModel.expenses
                            |> Computation.update model.computation

                    else
                        ( model.computation, Cmd.none )
            in
            ( { model
                | expense = newExpenseModel
                , computation = newComputationModel
              }
            , Cmd.batch [ Cmd.map ExpenseMsg newExpensesCmd, Cmd.map ComputationMsg newComputationCmd ]
            )

        ComputationMsg computationMsg ->
            let
                ( newComputationModel, newComputationCmd ) =
                    Computation.update model.computation computationMsg
            in
            ( { model | computation = newComputationModel }
            , Cmd.map ComputationMsg newComputationCmd
            )

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
