module Locale exposing (..)

import Amount
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Locale =
    { amount : Amount.Locale
    }


decoder : Decoder Locale
decoder =
    Decode.map
        Locale
        (Decode.field "a" <| Amount.localeDecoder)
        
encode : Locale -> Encode.Value
encode values =
    [ ( "a", values.amount |> Amount.encodeLocale )
    ]
        |> Encode.object
