module Util.String exposing (..)

import Round


fromAmount : Float -> String
fromAmount =
    Round.round 2


fromAmountSigned : Float -> String
fromAmountSigned amount =
    fromAmount amount
        |> (\string ->
                if amount > 0 then
                    "+" ++ string

                else
                    string
           )
