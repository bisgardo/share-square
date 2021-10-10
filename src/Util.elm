module Util exposing (..)

import Dict exposing (Dict)


chainUpdate : msg -> (msg -> a -> ( a, Cmd msg )) -> ( a, Cmd msg ) -> ( a, Cmd msg )
chainUpdate msg update ( model, cmd ) =
    update msg model
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])


parseDictKeys : (a -> Result x comparable) -> Dict a c -> Result x (Dict comparable c)
parseDictKeys parse =
    Dict.foldl
        (\key value ->
            Result.andThen
                (\dict ->
                    parse key
                        |> Result.map (\parsedKey -> Dict.insert parsedKey value dict)
                )
        )
        (Ok Dict.empty)


sumDictValues : Dict comparable number -> Dict comparable number -> Dict comparable number
sumDictValues source target =
    Dict.foldl
        (\key value ->
            Dict.update key (Maybe.withDefault 0 >> (+) value >> Just)
        )
        target
        source
