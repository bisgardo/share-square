module Util.JsonDecode exposing (..)

import Json.Decode as Decode


nullableList : Decode.Decoder a -> Decode.Decoder (List a)
nullableList =
    Decode.list
        >> Decode.maybe
        >> Decode.map (Maybe.withDefault [])
