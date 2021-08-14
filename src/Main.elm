module Main exposing (main)

import Browser
import Html


type alias Flags =
    { environment : String
    }


main : Program Flags Flags ()
main =
    Browser.document
        { init = \flags -> ( flags, Cmd.none )
        , view = \model -> { title = "Share-square", body = [ Html.text <| "Environment: " ++ model.environment ] }
        , update = \() model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        }
