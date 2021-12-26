module Config exposing (..)

import Amount
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Config =
    { amount : Amount.Locale
    }


decoder : Decoder Config
decoder =
    Decode.map
        Config
        (Decode.field "a" <| Amount.localeDecoder)
        
encode : Config -> Encode.Value
encode values =
    [ ( "a", values.amount |> Amount.encodeLocale )
    ]
        |> Encode.object
