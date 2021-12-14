module Participant exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Layout exposing (..)
import Maybe.Extra as Maybe
import Set exposing (Set)
import Util.Update as Update


type alias Participant =
    { id : Int
    , name : String
    , nameLowercase : String -- used for case-insensitive sorting
    }


decoder : Decoder Participant
decoder =
    Decode.map2
        new
        -- ID
        (Decode.field "i" Decode.int)
        -- name
        (Decode.field "n" Decode.string)


encode : Participant -> Value
encode participant =
    [ ( "i", participant.id |> Encode.int )
    , ( "n", participant.name |> Encode.string )
    ]
        |> Encode.object


toField : Participant -> Field
toField participant =
    { key = String.fromInt participant.id
    , value = participant.name
    }


lookupName : Int -> Dict Int String -> String
lookupName id idToName =
    case Dict.get id idToName of
        Nothing ->
            "<" ++ String.fromInt id ++ ">"

        Just name ->
            name


type alias Model =
    { create : Maybe CreateModel
    , participants : List Participant
    , idToName : Dict Int String
    , namesLowercase : Set String -- used for case-insensitive duplication check
    , nextId : Int
    }


type alias CreateModel =
    { name : Validated Field
    }


init : ( Model, Cmd Msg )
init =
    ( { create = Nothing
      , participants = []
      , idToName = Dict.empty
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
        [ onSubmit CreateSubmit
        ]
        [ modal createModalId "Add participant" body disable
        ]


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
                        -- Keep the participant list sorted by name (case insensitively).
                        participants =
                            model.participants ++ [ value ] |> List.sortBy .nameLowercase
                    in
                    ( ( { model
                            | participants = participants
                            , idToName = model.idToName |> Dict.insert id value.name
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
        , idToName =
            participants
                |> List.foldl (\participant -> Dict.insert participant.id participant.name) Dict.empty
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


create : Int -> String -> Result String Participant
create id name =
    -- Should probably run the name through the validator...
    if name |> String.isEmpty then
        Err "cannot create participant with empty name"

    else
        Ok <| new id (name |> cleanName)


new : Int -> String -> Participant
new id name =
    { id = id
    , name = name
    , nameLowercase = name |> String.toLower
    }


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
