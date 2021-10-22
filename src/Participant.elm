module Participant exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, value)
import Html.Events exposing (onInput, onSubmit)
import Layout exposing (..)


type alias Participant =
    { id : Int
    , name : String
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
    { create : CreateModel
    , participants : List Participant
    , names : Dict Int String
    , nextId : Int
    }


type alias CreateModel =
    { name : String
    }


init : Model
init =
    { create = initCreate
    , participants = []
    , names = Dict.empty
    , nextId = 1
    }


initCreate : CreateModel
initCreate =
    { name = ""
    }


type Msg
    = CreateUpdate String
    | CreateSubmit


createId : String
createId =
    "participant-create-input"


view : Model -> Html Msg
view model =
    row
        [ model.participants |> List.map .name |> (row1 << col << List.map (row1 << col1 << text))
        , model.create |> (row1 << col1 << viewCreate)
        ]


viewCreate : CreateModel -> Html Msg
viewCreate model =
    Html.form
        [ onSubmit CreateSubmit
        ]
        [ div
            [ class "input-group"
            ]
            [ Html.input
                [ id createId
                , class "form-control"
                , onInput CreateUpdate
                , value model.name
                ]
                []
            , Html.button
                [ class "btn btn-primary"
                , String.isEmpty model.name |> Html.Attributes.disabled
                ]
                [ text "Add" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateUpdate name ->
            ( { model
                | create = { name = name }
              }
            , Cmd.none
            )

        CreateSubmit ->
            case model.create.name of
                "" ->
                    let
                        _ =
                            Debug.log "error" "cannot create participant with empty name"
                    in
                    ( model, Cmd.none )

                rawName ->
                    let
                        id =
                            model.nextId

                        name =
                            rawName |> cleanName

                        newNextId =
                            model.nextId + 1
                    in
                    ( { model
                        | participants =
                            model.participants
                                ++ [ { id = id, name = name } ]
                                -- Keep the participant list sorted by name.
                                |> List.sortBy (.name >> String.toLower)
                        , names = model.names |> Dict.insert id name
                        , create = initCreate
                        , nextId = newNextId
                      }
                    , Cmd.none
                    )


cleanName : String -> String
cleanName =
    String.uncons
        >> Maybe.map
            (\( first, rest ) ->
                String.toUpper (String.fromChar first) ++ rest
            )
        >> Maybe.withDefault ""
