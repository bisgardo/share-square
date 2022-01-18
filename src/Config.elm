module Config exposing (..)

import Domain.Amount as Amount


type alias Config =
    { amount : Amount.Config
    }


defaultDecimalSeparator =
    "."


{-| To be expanded to include display strings.
-}
default : Config
default =
    -- TODO Add triviality limit (no payments with amounts lower than this will be suggested and amounts will be rendered as "~0.00").
    { amount =
        --{ decimalPlaces = 3
        --, decimalSeparator = ","
        --}
        { decimalPlaces = 2
        , decimalSeparator = defaultDecimalSeparator
        }
    }
