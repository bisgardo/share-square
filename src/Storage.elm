module Storage exposing (..)

import LocalStorage exposing (Revision)
import Maybe.Extra as Maybe


type Mode
    = None
    | Local


{-| Send request to store value with the provided ID in storage.
-}
storeValue : String -> Revision -> String -> Mode -> Cmd msg
storeValue id revision value mode =
    case mode of
        None ->
            Cmd.none

        Local ->
            LocalStorage.storeValue ( id, revision, value )


{-| Subscription to listen for value stored to storage.
-}
valueStored : (( String, Result String Revision ) -> msg) -> Mode -> Sub msg
valueStored handler mode =
    case mode of
        None ->
            Sub.none

        Local ->
            LocalStorage.valueStored
                (\( id, revision, error ) ->
                    let
                        result =
                            error
                                |> Maybe.unwrap (Ok revision) Err
                    in
                    handler ( id, result )
                )


{-| Send request to load value with the provided ID from storage.
-}
loadValues : String -> Mode -> Cmd msg
loadValues id mode =
    case mode of
        None ->
            Cmd.none

        Local ->
            LocalStorage.loadValue id


{-| Subscription to listen for value loaded from storage.
-}
valueLoaded : (Maybe ( String, Revision, String ) -> msg) -> Mode -> Sub msg
valueLoaded handler mode =
    case mode of
        None ->
            Sub.none

        Local ->
            LocalStorage.valueLoaded handler


{-| Send request to delete value from storage.
-}
deleteValue : String -> Mode -> Cmd msg
deleteValue id mode =
    case mode of
        None ->
            Cmd.none

        Local ->
            LocalStorage.deleteValue id
