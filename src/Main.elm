module Main exposing (main)

import Browser
import Computation
import Expense
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Layout exposing (..)


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
    let
        ( expenseModel, expenseCmd ) =
            Expense.init

        ( computationModel, computationCmd ) =
            Computation.init
    in
    ( { environment = flags.environment
      , expense = expenseModel
      , computation = computationModel
      }
    , Cmd.batch
        [ expenseCmd |> Cmd.map ExpenseMsg
        , computationCmd |> Cmd.map ComputationMsg
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ model.expense
        |> Expense.subscriptions
        |> Sub.map ExpenseMsg
    , model.computation
        |> Computation.subscriptions
        |> Sub.map ComputationMsg
    ]
        |> Sub.batch


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
                ]
                [ Html.text "Expenses" ]
            ]
        , Html.li [ Html.Attributes.class "nav-item" ]
            [ let
                disableSettlementTab =
                    model.expense.expenses |> List.isEmpty
              in
              if disableSettlementTab then
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
                    , data "bs-toggle" "tab"
                    , data "bs-target" ("#" ++ tabIds.settlement)
                    , Html.Attributes.class "nav-link"
                    , Computation.Enable
                        (model.expense.participant.participants |> List.map .id)
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
            (viewExpenses model)
        , Html.div
            [ Html.Attributes.id tabIds.settlement
            , Html.Attributes.class "tab-pane fade"
            ]
            (viewComputation model)
        ]
    ]


viewExpenses : Model -> List (Html Msg)
viewExpenses model =
    model.expense
        |> Expense.view
        |> List.map (Html.map ExpenseMsg)


viewComputation : Model -> List (Html Msg)
viewComputation model =
    model.computation
        |> Computation.view model.expense.participant
        |> Html.map ComputationMsg
        |> List.singleton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExpenseMsg expenseMsg ->
            let
                ( ( newExpenseModel, recompute ), newExpensesCmd ) =
                    model.expense |> Expense.update expenseMsg

                ( newComputationModel, newComputationCmd ) =
                    if recompute then
                        model.computation
                            |> Computation.update Computation.Disable

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
                    model.computation |> Computation.update computationMsg
            in
            ( { model | computation = newComputationModel }
            , newComputationCmd |> Cmd.map ComputationMsg
            )
