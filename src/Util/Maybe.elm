module Util.Maybe exposing (..)


nothingIf : (a -> Bool) -> Maybe a -> Maybe a
nothingIf predicate =
    Maybe.andThen
        (\value ->
            if value |> predicate then
                Nothing

            else
                Just value
        )
