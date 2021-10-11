module Util.List exposing (..)

ifNonEmpty : List a -> List b -> List b
ifNonEmpty test result =
    if List.isEmpty test then
        []

    else
        result
