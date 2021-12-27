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
Note that the subscription shouldn't depend on the storage mode
as that would result in a race condition between setting the mode and
registering the subscription, leading to lost messages and tears.
-}
valueStored : (( String, Result Revision Revision ) -> msg) -> Sub msg
valueStored handler =
    LocalStorage.valueStored
        (\( id, revision, error ) ->
            handler ( id, error |> Maybe.unwrap (Ok revision) Err )
        )


{-| Send request to load value with the provided ID from storage.
-}
loadValue : String -> Mode -> Cmd msg
loadValue id mode =
    case mode of
        None ->
            Cmd.none

        Local ->
            LocalStorage.loadValue id


{-| Subscription to listen for value loaded from storage.
Note that the subscription shouldn't depend on the storage mode
as that would result in a race condition between setting the mode and
registering the subscription, leading to lost messages and tears.
-}
valueLoaded : (Maybe ( String, Revision, String ) -> msg) -> Sub msg
valueLoaded =
    LocalStorage.valueLoaded


{-| Send request to delete value from storage.
-}
deleteValue : String -> Mode -> Cmd msg
deleteValue id mode =
    case mode of
        None ->
            Cmd.none

        Local ->
            LocalStorage.deleteValue id
