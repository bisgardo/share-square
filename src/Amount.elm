module Amount exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Amount =
    Int


type alias Locale =
    { decimalPlaces : Int
    , decimalSeparator : String
    , otherSeparator : String
    }


fromString : Locale -> String -> Maybe Amount
fromString locale string =
    string
        |> String.replace locale.otherSeparator ""
        |> String.replace locale.decimalSeparator "."
        |> String.toFloat
        |> Maybe.map ((*) (10.0 ^ toFloat locale.decimalPlaces) >> round)


toString : Locale -> Amount -> String
toString locale amount =
    let
        string =
            amount |> String.fromInt |> String.padLeft (locale.decimalPlaces + 1) '0'

        right =
            string |> String.right locale.decimalPlaces

        left =
            string |> String.dropRight locale.decimalPlaces
    in
    left ++ locale.decimalSeparator ++ right


toStringSigned : Locale -> Amount -> String
toStringSigned locale amount =
    let
        string =
            toString locale amount
    in
    if amount > 0 then
        "+" ++ string

    else
        string


decoder : Decoder Amount
decoder =
    Decode.int


encode : Amount -> Encode.Value
encode =
    Encode.int
