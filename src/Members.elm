module Members exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onSubmit)
import Layout exposing (..)


type alias Name =
    String


type alias Model =
    { names : List Name
    , input : String
    }


type Msg
    = CreateUpdate String
    | CreateSubmit


view : Model -> Html Msg
view model =
    row
        [ model.names |> (row1 << col << List.map (row1 << col1 << text))
        , model |> (row1 << col1 << viewCreate)
        ]


viewCreate : Model -> Html Msg
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
                , value model.input
                ]
                []
            , Html.button
                [ class "btn btn-primary"
                ]
                [ text "Add" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateUpdate input ->
            ( { model | input = input }
            , Cmd.none
            )

        CreateSubmit ->
            ( { model | names = model.names ++ [ model.input ], input = "" }
            , Cmd.none
            )
