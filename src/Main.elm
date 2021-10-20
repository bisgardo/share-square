module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Computation
import Expenses
import Html exposing (Html)
import Layout exposing (..)
import Members
import Task
import Util.List as List


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , members : Members.Model
    , expenses : Expenses.Model
    , computation : Computation.Model
    }


type Msg
    = MemberMsg Members.Msg
    | ExpenseMsg Expenses.Msg
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
      , members = Members.init
      , expenses = Expenses.init
      , computation = Computation.init
      }
    , Dom.focus Members.createId |> Task.attempt DomMsg
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.expenses
        |> Expenses.subscriptions
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
            [ viewMembers model
            , viewExpenses model
            , viewComputation model
            ]


viewMembers : Model -> List (Html Msg)
viewMembers model =
    [ Html.h1 [] [ Html.text "Members" ]
    , model.members
        |> Members.view
        |> Html.map MemberMsg
    ]


viewExpenses : Model -> List (Html Msg)
viewExpenses model =
    List.ifNonEmpty model.members.members <|
        [ Html.h1 [] [ Html.text "Expenses" ]
        , model.expenses
            |> Expenses.view model.members
            |> Html.map ExpenseMsg
        ]


viewComputation : Model -> List (Html Msg)
viewComputation model =
    List.ifNonEmpty model.expenses.expenses
        [ model.computation
            |> Computation.view model.members.names
            |> Html.map ComputationMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MemberMsg memberMsg ->
            let
                ( newMembersModel, newMembersCmd ) =
                    Members.update memberMsg model.members
            in
            ( { model | members = newMembersModel }
            , Cmd.map MemberMsg newMembersCmd
            )

        ExpenseMsg expenseMsg ->
            let
                ( ( newExpensesModel, recompute ), newExpensesCmd ) =
                    Expenses.update model.members.members expenseMsg model.expenses

                ( newComputationModel, newComputationCmd ) =
                    if recompute then
                        Computation.Recompute model.members.members newExpensesModel.expenses
                            |> Computation.update model.computation

                    else
                        ( model.computation, Cmd.none )
            in
            ( { model
                | expenses = newExpensesModel
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
