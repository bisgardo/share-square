module Main exposing (main)

import Browser
import Html exposing (Html)
import Layout exposing (..)
import Members


type alias Flags =
    { environment : String
    }


type alias Model =
    { environment : String
    , members : Members.Model
    }


type Msg
    = MemberMsg Members.Msg


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
      , members = { names = [], input = "" }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Share-square"
    , body = [ model |> viewBody ]
    }


viewBody : Model -> Html Msg
viewBody model =
    container <|
        [ Html.h1 [] [ Html.text "Members" ]
        , viewMembers model
        ]


viewMembers : Model -> Html Msg
viewMembers model =
    model.members
        |> Members.view
        |> Html.map MemberMsg


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
