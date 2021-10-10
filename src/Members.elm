module Members exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onSubmit)
import Layout exposing (..)


type alias Member =
    { id : Int
    , name : String
    }


type alias Model =
    { create : CreateModel
    , members : List Member
    , nextMemberId : Int
    }


type alias CreateModel =
    { name : String
    }


init : Model
init =
    { create = initCreate
    , members = []
    , nextMemberId = 1
    }


initCreate : CreateModel
initCreate =
    { name = ""
    }


type Msg
    = CreateUpdate String
    | CreateSubmit


view : Model -> Html Msg
view model =
    row
        [ model.members |> List.map .name |> (row1 << col << List.map (row1 << col1 << text))
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
                [ class "form-control"
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
                            Debug.log "error" "cannot create member with empty name"
                    in
                    ( model, Cmd.none )

                name ->
                    ( { model
                        | members = model.members ++ [ { id = model.nextMemberId, name = name } ]
                        , create = initCreate
                        , nextMemberId = model.nextMemberId + 1
                      }
                    , Cmd.none
                    )
