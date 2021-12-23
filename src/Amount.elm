module Amount exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Amount =
    Int


type alias Locale =
    { decimalPlaces : Int
    , decimalSeparator : String
    }


toDecimalPoint : String -> String -> String
toDecimalPoint separator =
    if separator == "." then
        identity

    else
        String.replace "." "!" >> String.replace separator "."


fromString : Locale -> String -> Maybe Amount
fromString locale string =
    string
        |> toDecimalPoint locale.decimalSeparator
        |> String.toFloat
        |> Maybe.map ((*) (10 ^ toFloat locale.decimalPlaces) >> round)


toString : Locale -> Amount -> String
toString =
    toStringSigned ""


toStringSigned : String -> Locale -> Amount -> String
toStringSigned positiveSign locale amount =
    let
        ( amountSign, prefix ) =
            if amount < 0 then
                ( -1, "-" )

            else
                ( 1, positiveSign )

        string =
            amount * amountSign |> String.fromInt |> String.padLeft (locale.decimalPlaces + 1) '0'

        right =
            string |> String.right locale.decimalPlaces

        left =
            string |> String.dropRight locale.decimalPlaces
    in
    prefix ++ left ++ locale.decimalSeparator ++ right


decoder : Decoder Amount
decoder =
    Decode.int


encode : Amount -> Encode.Value
encode =
    Encode.int
