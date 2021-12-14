port module LocalStorage exposing (..)

-- TODO Move 'Revision' to somewhere more generic.


type alias Revision =
    Int


{-| Send request to load value with the provided ID from local storage.
-}
port loadValue : String -> Cmd msg


{-| Subscription to listen for value loaded from local storage.
-}
port valueLoaded : (Maybe ( String, Revision, String ) -> msg) -> Sub msg


{-| Send request to store value with the provided ID in local storage.
-}
port storeValue : ( String, Revision, String ) -> Cmd msg


{-| Subscription to listen for the revision and error from storing a value in local storage.
-}
port valueStored : (( String, Revision, Maybe Int ) -> msg) -> Sub msg


{-| Send request to delete value from local storage.
-}
port deleteValue : String -> Cmd msg
