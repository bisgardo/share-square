module Participant exposing (..)

import Dict exposing (Dict)
import Domain.Participant as Participant exposing (Participant)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onSubmit)
import Layout exposing (..)
import Maybe.Extra as Maybe
import Set exposing (Set)
import Util.Update as Update


toField : Participant -> Field
toField participant =
    { key = participant.id |> Participant.idToString
    , value = participant.name
    }


type alias Model =
    { create : Maybe CreateModel
    , participants : List Participant -- TODO change to list of IDs and make it sorted again
    , idToIndex : Participant.Index -- TODO change to dict from ID to participant (will make lookup "single-jump")
    , namesLowercase : Set String -- used for case-insensitive duplication check
    , nextId : Participant.Id
    }


type alias CreateModel =
    { name : Validated Field
    }


init : ( Model, Cmd Msg )
init =
    ( { create = Nothing
      , participants = []
      , idToIndex = Dict.empty
      , namesLowercase = Set.empty
      , nextId = 1
      }
    , Cmd.none
    )


initCreate : CreateModel
initCreate =
    { name =
        { key = "new-participant-name"
        , value = ""
        , feedback = None
        }
    }


type Msg
    = LoadCreate
    | CreateUpdate String
    | CreateSubmit
    | CloseModal


createId : String
createId =
    "participant-create-open"


createModalId : String
createModalId =
    "participant-create"


viewCreateOpen : Html Msg
viewCreateOpen =
    -- Wrapping button in span because the same element
    -- cannot be used to toggle both tooltip and modal.
    Html.span
        [ data "bs-toggle" "tooltip"
        , data "bs-placement" "left"
        , Html.Attributes.title "Add participant"
        ]
        [ openModalButton
            createId
            createModalId
            "+"
            [ Html.Events.onClick LoadCreate
            , Html.Attributes.class "btn-sm"
            ]
        ]


viewCreateModal : Model -> Html Msg
viewCreateModal model =
    let
        ( body, disable ) =
            case model.create of
                Nothing ->
                    ( [ text "Loading..." ], True )

                Just createModel ->
                    ( [ textInput "Name" createModel.name CreateUpdate ]
                    , (createModel.name.value |> String.trim |> String.isEmpty)
                        || List.any isInvalid [ createModel.name ]
                    )
    in
    Html.form
        [ onSubmit CreateSubmit ]
        [ modal createModalId "Add participant" body disable Nothing ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( ( Model, Bool ), Cmd Msg )
update msg model =
    case msg of
        LoadCreate ->
            ( ( { model
                    | create =
                        Just initCreate
                }
              , False
              )
            , Cmd.none
            )

        CreateUpdate name ->
            ( ( { model
                    | create =
                        model.create
                            |> Maybe.map
                                (\createModel ->
                                    let
                                        nameField =
                                            createModel.name
                                    in
                                    { createModel
                                        | name =
                                            { nameField
                                                | value = name
                                                , feedback = validateName model name
                                            }
                                    }
                                )
                }
              , False
              )
            , Cmd.none
            )

        CreateSubmit ->
            let
                id =
                    model.nextId

                participant =
                    model.create
                        |> Result.fromMaybe "no create model found"
                        |> Result.map (.name >> .value >> String.trim)
                        |> Result.andThen (create id)
            in
            case participant of
                Err error ->
                    -- TODO Print error on page.
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( ( model, False ), Cmd.none )

                Ok value ->
                    let
                        participants =
                            model.participants ++ [ value ]
                    in
                    ( ( { model
                            | participants = participants
                            , idToIndex = model.idToIndex |> Dict.insert id (model.participants |> List.length)
                            , namesLowercase = model.namesLowercase |> Set.insert value.nameLowercase
                            , create = Nothing
                            , nextId = id + 1
                        }
                      , True
                      )
                    , Update.delegate CloseModal
                    )

        CloseModal ->
            ( ( model, False ), closeModal createModalId )


import_ : List Participant -> Model -> Model
import_ participants model =
    { model
        | participants = participants
        , idToIndex =
            participants
                |> List.foldl (\participant result -> Dict.insert participant.id (result |> Dict.size) result) Dict.empty
        , namesLowercase =
            participants
                |> List.map (.name >> String.toLower)
                |> Set.fromList
        , create = Nothing
        , nextId =
            1
                + (participants
                    |> List.foldl (\participant -> max participant.id) (model.nextId - 1)
                  )
    }


create : Participant.Id -> String -> Result String Participant
create id name =
    -- Should probably run the name through the validator...
    if name |> String.isEmpty then
        Err "cannot create participant with empty name"

    else
        Ok <| Participant.new id (name |> cleanName)


cleanName : String -> String
cleanName =
    String.trim
        >> String.uncons
        >> Maybe.unwrap
            ""
            (\( first, rest ) ->
                String.toUpper (String.fromChar first) ++ rest
            )


validateName : Model -> String -> Feedback
validateName model name =
    if model.namesLowercase |> Set.member (name |> String.toLower) then
        Error "Duplicate name."

    else
        None
