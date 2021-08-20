module Main exposing (main)

import Browser
import Html


type alias Flags =
    { environment : String
    }


type alias Model =
    Flags


type alias Msg =
    ()


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
    ( flags, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Share-square"
    , body =
        [ Html.text <| "Environment: " ++ model.environment ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
