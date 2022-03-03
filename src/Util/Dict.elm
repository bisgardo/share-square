module Util.Dict exposing (..)

import Dict exposing (Dict)
import Maybe.Extra as Maybe


parseKeys : (a -> Result x comparable) -> Dict a c -> Result x (Dict comparable c)
parseKeys parse =
    Dict.foldl
        (\key value ->
            Result.andThen
                (\dict ->
                    parse key
                        |> Result.map (\parsedKey -> Dict.insert parsedKey value dict)
                )
        )
        (Ok Dict.empty)


sumValues : Dict comparable number -> Dict comparable number -> Dict comparable number
sumValues fewestKeys mostKeys =
    Dict.foldl
        (\key value ->
            Dict.update key (Maybe.withDefault 0 >> (+) value >> Just)
        )
        mostKeys
        fewestKeys


valueSum : comparable -> Dict comparable (Dict b number) -> number
valueSum key =
    Dict.get key
        >> Maybe.unwrap 0 (Dict.values >> List.sum)
