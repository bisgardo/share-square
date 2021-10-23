module Participant exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onSubmit)
import Layout exposing (..)
import Maybe.Extra as Maybe
import Set exposing (Set)
import Util.Update as Update


type alias Participant =
    { id : Int
    , name : String -- TODO require unique names
    }


toField : Participant -> Field
toField participant =
    { key = String.fromInt participant.id
    , value = participant.name
    }


lookupName : Int -> Dict Int String -> String
lookupName id =
    Dict.get id
        >> Maybe.withDefault ("<" ++ String.fromInt id ++ ">")


type alias Model =
    { create : Maybe CreateModel
    , participants : List Participant
    , names : Dict Int String
    , lowercaseNames : Set String
    , nextId : Int
    }


type alias CreateModel =
    { name : Validated Field
    }


init : Model
init =
    { create = Nothing
    , participants = []
    , names = Dict.empty
    , lowercaseNames = Set.empty
    , nextId = 1
    }


initCreate : CreateModel
initCreate =
    { name =
        { key = "new-participant-name"
        , value = ""
        , validationError = Nothing
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
                    , String.isEmpty (createModel.name.value |> String.trim)
                        || Maybe.isJust createModel.name.validationError
                    )
    in
    Html.form
        [ onSubmit CreateSubmit
        ]
        [ modal createModalId "Add participant" body disable
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadCreate ->
            ( { model
                | create =
                    Just initCreate
              }
            , Cmd.none
            )

        CreateUpdate name ->
            ( { model
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
                                            , validationError = validateName model name
                                        }
                                }
                            )
              }
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
                    ( model, Cmd.none )

                Ok value ->
                    ( { model
                        | participants =
                            model.participants
                                ++ [ value ]
                                -- Keep the participant list sorted by name (case insensitively).
                                |> List.sortBy (.name >> String.toLower)
                        , names = model.names |> Dict.insert id value.name
                        , lowercaseNames = model.lowercaseNames |> Set.insert (value.name |> String.toLower)
                        , create = Nothing
                        , nextId = id + 1
                      }
                    , Update.delegate CloseModal
                    )

        CloseModal ->
            ( model, closeModal createModalId )


create : Int -> String -> Result String Participant
create id name =
    -- Should probably run the name through the validator...
    if String.isEmpty name then
        Err "cannot create participant with empty name"

    else
        Ok
            { id = id
            , name = name |> cleanName
            }


cleanName : String -> String
cleanName =
    String.trim
        >> String.uncons
        >> Maybe.map
            (\( first, rest ) ->
                String.toUpper (String.fromChar first) ++ rest
            )
        >> Maybe.withDefault ""


validateName : Model -> String -> Maybe String
validateName model name =
    if model.lowercaseNames |> Set.member (name |> String.toLower) then
        Just "Duplicate name."

    else
        Nothing
