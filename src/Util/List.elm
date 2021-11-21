module Util.List exposing (..)


withoutFirstMatch : (a -> Bool) -> List a -> ( Maybe a, List a )
withoutFirstMatch test list =
    case list of
        [] ->
            ( Nothing, list )

        first :: rest ->
            if test first then
                ( Just first, rest )

            else
                withoutFirstMatch test rest |> Tuple.mapSecond ((::) first)
