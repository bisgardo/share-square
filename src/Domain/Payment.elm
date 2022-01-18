module Domain.Payment exposing (..)

import Domain.Amount as Amount exposing (Amount)
import Domain.Participant as Participant
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Id =
    Int


idFromString : String -> Maybe Id
idFromString =
    String.toInt


idToString : Id -> String
idToString =
    String.fromInt


type alias Payment =
    { id : Id
    , payer : Participant.Id
    , receiver : Participant.Id
    , amount : Amount
    , done : Bool
    }


decoder : Decoder Payment
decoder =
    Decode.map5
        (\id payerId amount receiverId done ->
            { id = id
            , payer = payerId
            , amount = amount
            , receiver = receiverId
            , done = done
            }
        )
        -- ID
        (Decode.field "i" Decode.int)
        -- payer ID
        (Decode.field "p" Decode.int)
        -- amount
        (Decode.field "a" Amount.decoder)
        -- receiver ID
        (Decode.field "r" Decode.int)
        -- done?
        (Decode.field "d" <| Decode.bool)


encode : Payment -> Value
encode payment =
    [ ( "i", payment.id |> Encode.int )
    , ( "p", payment.payer |> Encode.int )
    , ( "a", payment.amount |> Amount.encode )
    , ( "r", payment.receiver |> Encode.int )
    , ( "d", payment.done |> Encode.bool )
    ]
        |> Encode.object


normalize : Payment -> Payment
normalize result =
    if result.amount < 0 then
        { result
            | payer = result.receiver
            , receiver = result.payer
            , amount = -result.amount
        }

    else
        result
