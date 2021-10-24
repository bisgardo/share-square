module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Computation
import Expense
import Html exposing (Html)
import Html.Attributes
import Layout exposing (..)
import Participant
import Task
import Util.List as List


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , expense : Expense.Model
    , computation : Computation.Model
    }


type Msg
    = ExpenseMsg Expense.Msg
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
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    container <|
        [ Html.div [ Html.Attributes.class "mb-4" ]
            [ Html.h1 [ Html.Attributes.class "d-inline" ] [ Html.text "Share 'n square" ]
            , Html.p [ Html.Attributes.class "lead d-inline ms-2" ] [ Html.text "Expense calculator" ]
            ]
        ]
            ++ List.concat
                [ viewExpenses model
                , viewComputation model
                ]


viewExpenses : Model -> List (Html Msg)
viewExpenses model =
    [ Html.h2 [] [ Html.text "Expenses" ]
    , model.expense
        |> Expense.view
        |> Html.map ExpenseMsg
    ]


viewComputation : Model -> List (Html Msg)
viewComputation model =
    List.ifNonEmpty model.expense.expenses
        [ Html.h2 [] [ Html.text "Computation" ]
        , model.computation
            |> Computation.view model.expense.participant.names
            |> Html.map ComputationMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpenseMsg expenseMsg ->
            let
                ( ( newExpenseModel, recompute ), newExpensesCmd ) =
                    Expense.update expenseMsg model.expense

                ( newComputationModel, newComputationCmd ) =
                    if recompute then
                        Computation.Recompute (model.expense.participant.participants |> List.map .id) newExpenseModel.expenses
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
