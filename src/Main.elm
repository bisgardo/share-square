module Main exposing (main)

import Browser
import Computation
import Expenses
import Html exposing (Html)
import Layout exposing (..)
import Members


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , members : Members.Model
    , expenses : Expenses.Model
    }


type Msg
    = MemberMsg Members.Msg
    | ExpenseMsg Expenses.Msg


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
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.expenses |> Expenses.subscriptions |> Sub.map ExpenseMsg


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
    if List.length model.members.members > 0 then
        [ Html.h1 [] [ Html.text "Expenses" ]
        , model.expenses
            |> Expenses.view model.members.members
            |> Html.map ExpenseMsg
        ]

    else
        []


viewComputation : Model -> List (Html Msg)
viewComputation model =
    if List.length model.expenses.expenses > 0 then
        [ Html.h1 [] [ Html.text "Computation" ]
        , Computation.view model.members.members model.expenses.expenses
        ]

    else
        []


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
                ( newExpensesModel, newExpensesCmd ) =
                    Expenses.update model.members.members expenseMsg model.expenses
            in
            ( { model | expenses = newExpensesModel }
            , Cmd.map ExpenseMsg newExpensesCmd
            )
