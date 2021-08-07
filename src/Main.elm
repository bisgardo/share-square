module Main exposing (main)

import Browser
import Html


main : Program () () ()
main =
    Browser.document
        { init = \() -> ( (), Cmd.none )
        , view = \() -> { title = "Share-square", body = [ Html.text "..." ] }
        , update = \() () -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }
