module Locale exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Locale =
    { decimalSeparator : String
    }


decoder : Decoder Locale
decoder =
    Decode.map
        Locale
        (Decode.field "s" <| Decode.string)


encode : Locale -> Encode.Value
encode values =
    [ ( "a", values.decimalSeparator |> Encode.string )
    ]
        |> Encode.object
