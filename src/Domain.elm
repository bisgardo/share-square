module Domain exposing (..)

import Amount exposing (Amount)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe



-- PARTICIPANT --


type alias Participant =
    { id : Int
    , name : String
    , nameLowercase : String -- used for case-insensitive sorting
    }


newParticipant : Int -> String -> Participant
newParticipant id name =
    { id = id
    , name = name
    , nameLowercase = name |> String.toLower
    }


participantDecoder : Decoder Participant
participantDecoder =
    Decode.map2
        newParticipant
        -- ID
        (Decode.field "i" Decode.int)
        -- name
        (Decode.field "n" Decode.string)


encodeParticipant : Participant -> Value
encodeParticipant participant =
    [ ( "i", participant.id |> Encode.int )
    , ( "n", participant.name |> Encode.string )
    ]
        |> Encode.object



-- EXPENSE --


type alias Expense =
    { id : Int
    , payer : Int
    , amount : Amount
    , description : String
    , receivers : Dict Int Float -- map from participant ID to fractional part
    }


expenseDecoder : Decoder Expense
expenseDecoder =
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


encodeExpense : Expense -> Value
encodeExpense expense =
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



-- PAYMENT --


type alias Payment =
    { id : Int
    , payer : Int
    , receiver : Int
    , amount : Amount
    , done : Bool
    }


paymentDecoder : Decoder Payment
paymentDecoder =
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


encodePayment : Payment -> Value
encodePayment payment =
    [ ( "i", payment.id |> Encode.int )
    , ( "p", payment.payer |> Encode.int )
    , ( "a", payment.amount |> Amount.encode )
    , ( "r", payment.receiver |> Encode.int )
    , ( "d", payment.done |> Encode.bool )
    ]
        |> Encode.object


normalizePayment : Payment -> Payment
normalizePayment result =
    if result.amount < 0 then
        { result
            | payer = result.receiver
            , receiver = result.payer
            , amount = -result.amount
        }

    else
        result


findSuggestedPayment : Dict Int Amount -> Maybe ( ( Int, Amount ), ( Int, Amount ) )
findSuggestedPayment =
    Dict.foldl
        (\participantId participantBalance result ->
            case result of
                Nothing ->
                    Just ( ( participantId, participantBalance ), ( participantId, participantBalance ) )

                Just ( ( _, minAmount ) as minResult, ( _, maxAmount ) as maxResult ) ->
                    if participantBalance < minAmount then
                        Just ( ( participantId, participantBalance ), maxResult )

                    else if participantBalance > maxAmount then
                        Just ( minResult, ( participantId, participantBalance ) )

                    else
                        result
        )
        Nothing


autosuggestPayments : Dict Int Amount -> Dict Int (List ( Int, Amount ))
autosuggestPayments totalBalances =
    case totalBalances |> autosuggestPayment of
        Nothing ->
            Dict.empty

        Just ( payerId, receiverId, amount ) ->
            totalBalances
                |> updatePaymentBalances payerId receiverId amount
                |> autosuggestPayments
                |> Dict.update payerId
                    (Maybe.withDefault []
                        >> (::) ( receiverId, amount )
                        >> Just
                    )


autosuggestPayment : Dict Int Amount -> Maybe ( Int, Int, Amount )
autosuggestPayment =
    findSuggestedPayment
        >> Maybe.andThen
            (\( ( minParticipant, minBalance ), ( maxParticipant, maxBalance ) ) ->
                let
                    debt =
                        min maxBalance -minBalance
                in
                -- TODO Should really do if |debt| < epsilon?
                if debt == 0 then
                    Nothing

                else
                    Just ( minParticipant, maxParticipant, debt )
            )


sumBalances : Int -> Dict Int Amount -> Dict Int Amount -> Amount
sumBalances participantId paymentBalance balance =
    (balance |> lookupBalance participantId) + (paymentBalance |> lookupBalance participantId)


lookupBalance : Int -> Dict Int Amount -> Amount
lookupBalance participantId =
    Dict.get participantId >> Maybe.withDefault 0


suggestPaymentAmount : String -> String -> Dict Int Amount -> Dict Int Amount -> Result ( Maybe Int, Maybe Int ) Amount
suggestPaymentAmount payer receiver paymentBalance balance =
    let
        payerId =
            payer |> String.toInt |> Maybe.withDefault 0

        receiverId =
            receiver |> String.toInt |> Maybe.withDefault 0

        payerBalance =
            sumBalances payerId paymentBalance balance

        receiverBalance =
            sumBalances receiverId paymentBalance balance

        suggestedAmount =
            min -payerBalance receiverBalance
    in
    if suggestedAmount <= 0 then
        Err
            ( if payerBalance >= 0 then
                Just payerId

              else
                Nothing
            , if receiverBalance <= 0 then
                Just receiverId

              else
                Nothing
            )

    else
        Ok suggestedAmount


updatePaymentBalances : Int -> Int -> Amount -> Dict Int Amount -> Dict Int Amount
updatePaymentBalances payerId receiverId amount =
    updatePaymentBalance payerId amount >> updatePaymentBalance receiverId -amount


updatePaymentBalance : Int -> Amount -> Dict Int Amount -> Dict Int Amount
updatePaymentBalance participantId amount =
    Dict.update participantId (Maybe.withDefault 0 >> (+) amount >> Just)
