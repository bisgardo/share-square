module Domain.Expense exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe




type alias Expense =
    { id : Int
    , payer : Int
    , amount : Amount
    , description : String
    , receivers : Dict Int Float -- map from participant ID to fractional part
    }


decoder : Decoder Expense
decoder =
    Decode.map5
        Expense
        -- ID
        (Decode.field "i" Decode.int)
        -- payer ID
        (Decode.field "p" Decode.int)
        -- amount
        (Decode.field "a" Decode.int)
        -- description
        (Decode.maybe (Decode.field "d" Decode.string) |> Decode.map (Maybe.withDefault ""))
        -- receivers
        (Decode.field "r"
            (Decode.list Decode.int
                |> Decode.map
                    (List.map (\id -> ( id, 1 )) >> Dict.fromList)
            )
        )


encode : Expense -> Value
encode expense =
    [ Just ( "i", expense.id |> Encode.int )
    , Just ( "p", expense.payer |> Encode.int )
    , Just ( "a", expense.amount |> Amount.encode )
    , if expense.description |> String.isEmpty then
        Nothing

      else
        Just ( "d", expense.description |> Encode.string )
    , Just
        ( "r"
        , expense.receivers
            |> Dict.toList
            |> List.map Tuple.first
            |> Encode.list Encode.int
        )
    ]
        |> Maybe.values
        |> Encode.object
