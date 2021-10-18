module Util.Dict exposing (..)

import Dict exposing (Dict)


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
sumValues source target =
    Dict.foldl
        (\key value ->
            Dict.update key (Maybe.withDefault 0 >> (+) value >> Just)
        )
        target
        source


toFlatList : Dict a (Dict b c) -> List ( a, b, c )
toFlatList =
    Dict.toList
        >> List.map
            (\( outerKey, innerDict ) ->
                innerDict
                    |> Dict.toList
                    |> List.map
                        (\( innerKey, value ) ->
                            ( outerKey, innerKey, value )
                        )
            )
        >> List.concat


valueSum : comparable -> Dict comparable (Dict b number) -> number
valueSum key =
    Dict.get key
        >> Maybe.map (Dict.values >> List.sum)
        >> Maybe.withDefault 0
