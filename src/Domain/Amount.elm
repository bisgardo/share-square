module Domain.Amount exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| Numbers larger than this value (~10^21) - or smaller than its negative counterpart - render with exponential notation.
-}
max =
    -- Could limit amounts to 'Number.MAX_SAFE_INTEGER' (~10^16) to ensure that calculations are exact,
    -- but intermediate values should also be checked then.
    3875820019684147199


type alias Amount =
    Int


type alias Config =
    { decimalPlaces : Int -- TODO should have a max allowed value (9?)
    , decimalSeparator : String
    }



-- Consider if the conversion functions should be optimized (e.g. precomputing 10^decimal_places) or the results be cached...


toDecimalPoint : String -> String -> String
toDecimalPoint separator =
    if separator == "." then
        identity

    else
        String.replace "." "!" >> String.replace separator "."


fromString : Config -> String -> Maybe Amount
fromString locale string =
    string
        |> toDecimalPoint locale.decimalSeparator
        |> String.toFloat
        |> Maybe.map ((*) (10 ^ toFloat locale.decimalPlaces) >> round)


toString : Config -> Amount -> String
toString =
    toStringSigned ""


toStringSigned : String -> Config -> Amount -> String
toStringSigned positiveSign locale amount =
    let
        ( amountSign, prefix ) =
            if amount < 0 then
                ( -1, "-" )

            else
                ( 1, positiveSign )
    in
    prefix
        ++ (if amount * amountSign > max then
                "∞"

            else
                let
                    string =
                        amount * amountSign |> String.fromInt |> String.padLeft (locale.decimalPlaces + 1) '0'

                    right =
                        string |> String.right locale.decimalPlaces

                    left =
                        string |> String.dropRight locale.decimalPlaces
                in
                left ++ locale.decimalSeparator ++ right
           )


decoder : Decoder Amount
decoder =
    Decode.int


encode : Amount -> Encode.Value
encode =
    Encode.int
