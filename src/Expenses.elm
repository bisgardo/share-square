module Expenses exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Layout exposing (..)
import Maybe.Extra as Maybe
import Members exposing (Member)
import Round
import Set exposing (Set)
import Util.Update as Update
import Util.Dict as Dict


type Msg
    = LoadCreate
    | CreateSubmit
    | CreateEditAmount String
    | CreateEditPayer String
    | CreateEditReceiver String Bool
    | CloseModal
    | LayoutMsg Layout.Msg


type alias Model =
    { create : Maybe CreateModel
    , expenses : List Expense
    }


{-| Model of the form for creating a new expense.
The strings in this type are really ints but keeping them as strings saves conversions to/from HTML.
-}
type alias CreateModel =
    { amount : Validated Field
    , payerId : String
    , receivers : Dict String Float
    }


type alias Expense =
    { payer : Int
    , amount : Float
    , receivers : Dict Int Float -- map from member ID to fractional part
    }


create : CreateModel -> Result String Expense
create model =
    let
        payerResult =
            model.payerId
                |> String.toInt
                |> Result.fromMaybe ("unexpected non-integer key '" ++ model.payerId ++ "' of payer")

        amountResult =
            model.amount.value
                |> String.toFloat
                |> Result.fromMaybe "cannot parse amount"

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
            { payer = payerId
            , amount = amount
            , receivers = receivers
            }
        )
        payerResult
        amountResult
        receiverResult


init : Model
init =
    { create = Nothing
    , expenses = []
    }


initCreate : String -> Dict String Float -> CreateModel
initCreate initPayerId initReceiverIds =
    { payerId = initPayerId
    , amount =
        { key = "new-expense-amount"
        , value = ""
        , validationError = Nothing
        }
    , receivers = initReceiverIds
    }


createModalId =
    "add-expense"


view : List Member -> Model -> Html Msg
view members model =
    row1 <|
        Html.form [ Html.Events.onSubmit CreateSubmit ] <|
            [ openModalButton createModalId "Add" LoadCreate
            , let
                ( body, disable ) =
                    case model.create of
                        Nothing ->
                            ( [ Html.text "Loading..." ], True )

                        Just createModel ->
                            ( viewAdd members createModel
                            , String.isEmpty createModel.amount.value
                                || Maybe.isJust createModel.amount.validationError
                            )
              in
              modal createModalId "Add expense" body disable
            ]
                ++ viewExpenses members model.expenses


viewAdd : List Member -> CreateModel -> List (Html Msg)
viewAdd members model =
    let
        membersFields =
            List.map Members.toField members
    in
    [ optionsInput "new-expense-payer" "Payer" membersFields model.payerId CreateEditPayer
    , textInput "Amount" model.amount CreateEditAmount
    , checkboxesInput "Receivers" membersFields (model.receivers |> Dict.keys |> Set.fromList) CreateEditReceiver
    ]


viewExpenses : List Member -> List Expense -> List (Html Msg)
viewExpenses members expenseModels =
    [ Html.table [ class "table" ] <|
        [ Html.thead []
            [ Html.tr []
                ([ Html.th [ Html.Attributes.scope "col" ] [ Html.text "Payer" ]
                 , Html.th [ Html.Attributes.scope "col" ] [ Html.text "Amount" ]
                 ]
                    ++ List.map (.name >> Html.text >> List.singleton >> Html.th [ Html.Attributes.scope "col" ]) members
                )
            ]
        , Html.tbody [] <|
            List.map
                (\expense ->
                    Html.tr []
                        ([ Html.td [] [ members |> Members.nameFromId expense.payer |> Html.text ]
                         , Html.td [] [ expense.amount |> Round.round 2 |> Html.text ]
                         ]
                            ++ List.map
                                (\member ->
                                    Html.td []
                                        (if Dict.member member.id expense.receivers then
                                            [ Html.text "✓" ]

                                         else
                                            []
                                        )
                                )
                                members
                        )
                )
                expenseModels
        ]
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    modalClosed ModalClosed |> Sub.map LayoutMsg


update : List Member -> Msg -> Model -> ( Model, Cmd Msg )
update members msg model =
    case msg of
        LoadCreate ->
            ( { model
                | create =
                    members
                        |> List.head
                        |> Maybe.map
                            (\firstMember ->
                                initCreate
                                    (firstMember.id |> String.fromInt)
                                    (members |> List.map (.id >> String.fromInt) |> List.map (\key -> ( key, 1.0 )) |> Dict.fromList)
                            )
              }
            , Cmd.none
            )

        CreateSubmit ->
            let
                expense =
                    model.create
                        |> Result.fromMaybe "no create model found"
                        |> Result.andThen create
            in
            case expense of
                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )

                Ok value ->
                    ( { model
                        | expenses =
                            model.expenses ++ [ value ]
                      }
                    , Cmd.none
                    )
                        |> Update.chain CloseModal (update members)

        CloseModal ->
            ( model, closeModal createModalId )

        CreateEditAmount amount ->
            ( { model
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
            , Cmd.none
            )

        CreateEditPayer payer ->
            ( { model
                | create =
                    model.create
                        |> Maybe.map (\createModel -> { createModel | payerId = payer })
              }
            , Cmd.none
            )

        CreateEditReceiver receiverKey checked ->
            ( { model
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
            , Cmd.none
            )

        LayoutMsg layoutMsg ->
            case layoutMsg of
                ModalClosed modalId ->
                    if modalId == createModalId then
                        ( { model | create = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )


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
