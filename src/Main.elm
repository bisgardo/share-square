module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Computation
import Expense
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Layout exposing (..)
import Maybe.Extra as Maybe
import Participant
import Task


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , expense : Expense.Model
    , computation : Computation.Model
    , state : State
    }


type State
    = Expenses
    | Settlement


type Msg
    = ExpenseMsg Expense.Msg
    | ComputationMsg Computation.Msg
    | SetState State
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
      , state = Expenses
      }
    , Dom.focus Participant.createId |> Task.attempt DomMsg
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.expense
        |> Expense.subscriptions
        |> Sub.map ExpenseMsg


tabIds =
    { expenses = "expenses"
    , settlement = "settlement"
    }


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
            ++ viewContent model


viewContent : Model -> List (Html Msg)
viewContent model =
    [ Html.ul [ Html.Attributes.class "nav nav-tabs mb-2" ]
        [ Html.li [ Html.Attributes.class "nav-item" ]
            [ Html.button
                [ Html.Attributes.type_ "button"
                , data "bs-toggle" "tab"
                , data "bs-target" ("#" ++ tabIds.expenses)
                , Html.Attributes.class "nav-link active"
                , SetState Expenses |> Html.Events.onClick
                ]
                [ Html.text "Expenses" ]
            ]
        , Html.li [ Html.Attributes.class "nav-item" ]
            [ Html.button
                ([ Html.Attributes.type_ "button"
                 , data "bs-toggle" "tab"
                 , data "bs-target" ("#" ++ tabIds.settlement)
                 , Html.Attributes.class "nav-link"
                 , SetState Settlement |> Html.Events.onClick
                 ]
                    ++ (if Maybe.isNothing model.computation.computed then
                            [ Html.Attributes.class " disabled" ]

                        else
                            []
                       )
                )
                [ if Maybe.isNothing model.computation.computed then
                    Html.i [] [ Html.text "Nothing to settle yet..." ]

                  else
                    Html.text "Settlement"
                ]
            ]
        ]
    , Html.div [ Html.Attributes.class "tab-content" ]
        [ Html.div
            [ Html.Attributes.id tabIds.expenses
            , Html.Attributes.class "tab-pane fade active show"
            ]
            [ viewExpenses model ]
        , Html.div
            [ Html.Attributes.id tabIds.settlement
            , Html.Attributes.class "tab-pane fade"
            ]
            [ viewComputation model ]
        ]
    ]


viewExpenses : Model -> Html Msg
viewExpenses model =
    model.expense
        |> Expense.view
        |> Html.map ExpenseMsg


viewComputation : Model -> Html Msg
viewComputation model =
    model.computation
        |> Computation.view model.expense.participant.idToName
        |> Html.map ComputationMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpenseMsg expenseMsg ->
            let
                ( ( newExpenseModel, recompute ), newExpensesCmd ) =
                    Expense.update expenseMsg model.expense

                ( newComputationModel, newComputationCmd ) =
                    if recompute then
                        Computation.Recompute
                            (model.expense.participant.participants |> List.map .id)
                            newExpenseModel.expenses
                            |> Computation.update model.computation

                    else
                        ( model.computation, Cmd.none )
            in
            ( { model
                | expense = newExpenseModel
                , computation = newComputationModel
              }
            , Cmd.batch
                [ newExpensesCmd |> Cmd.map ExpenseMsg
                , newComputationCmd |> Cmd.map ComputationMsg
                ]
            )

        ComputationMsg computationMsg ->
            let
                ( newComputationModel, newComputationCmd ) =
                    Computation.update model.computation computationMsg
            in
            ( { model | computation = newComputationModel }
            , newComputationCmd |> Cmd.map ComputationMsg
            )

        SetState state ->
            ( { model | state = state }, Cmd.none )

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
