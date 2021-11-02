module Util.List exposing (..)

import List.Extra as List


indexedFind : (a -> Bool) -> List a -> Maybe ( Int, a )
indexedFind test list =
    list
        |> List.findIndex test
        |> Maybe.andThen
            (\index ->
                List.getAt index list
                    |> Maybe.map (\element -> ( index, element ))
            )


withoutFirst : (a -> Bool) -> List a -> ( Maybe a, List a )
withoutFirst test list =
    case list of
        [] ->
            ( Nothing, list )

        first :: rest ->
            if test first then
                ( Just first, rest )

            else
                withoutFirst test rest |> Tuple.mapSecond ((::) first)
