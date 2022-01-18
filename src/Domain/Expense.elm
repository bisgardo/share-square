module Domain.Expense exposing (..)

import Dict exposing (Dict)
import Domain.Amount as Amount exposing (Amount)
import Domain.Balance exposing (Balances)
import Domain.Participant as Participant
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe
import Util.Dict as Dict


type alias Id =
    Int


idFromString : String -> Maybe Id
idFromString =
    String.toInt


idToString : Id -> String
idToString =
    String.fromInt


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


{-| A dict from ID of payer to dict from ID of receiver to totally expensed amount.
-}
type alias Expenses =
    Dict Participant.Id Balances


{-| A dict from ID of receiver to dict from ID of payer to totally expensed amount.
-}
type alias Debt =
    Expenses


expensesFromList : List Expense -> Expenses
expensesFromList =
    List.foldl
        (\expense outerResult ->
            let
                weightSum =
                    expense.receivers
                        |> Dict.values
                        |> List.sum

                weightedDebt =
                    expense.receivers
                        |> Dict.foldl
                            (\receiver part innerResult ->
                                if receiver == expense.payer then
                                    -- Ignore debt to self.
                                    innerResult

                                else
                                    innerResult
                                        |> Dict.insert receiver (part * (expense.amount |> toFloat) / weightSum |> round)
                            )
                            Dict.empty
            in
            if weightedDebt |> Dict.isEmpty then
                -- Ignore entry if the payer is the only receiver of the expense.
                outerResult

            else
                Dict.update expense.payer
                    (Maybe.withDefault Dict.empty
                        >> Dict.sumValues weightedDebt
                        >> Just
                    )
                    outerResult
        )
        Dict.empty


invert : Expenses -> Debt
invert =
    Dict.foldl
        (\payer payerExpenses result ->
            Dict.foldl
                (\receiver amount ->
                    Dict.update receiver
                        (Maybe.withDefault Dict.empty
                            >> Dict.update payer (Maybe.withDefault 0 >> (+) amount >> Just)
                            >> Just
                        )
                )
                result
                payerExpenses
        )
        Dict.empty
