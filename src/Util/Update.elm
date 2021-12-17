module Util.Update exposing (..)

import Task


chain : (msg -> a -> ( b, Cmd msg )) -> msg -> ( a, Cmd msg ) -> ( b, Cmd msg )
chain update msg ( model, cmd ) =
    update msg model
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])


chains : (msg -> a -> ( a, Cmd msg )) -> List msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
chains update msgs modelCmdPair =
    msgs |> List.foldl (chain update) modelCmdPair


withPairModel : (msg -> a -> ( ( b, y ), Cmd msg )) -> (x -> y -> z) -> msg -> ( a, x ) -> ( ( b, z ), Cmd msg )
withPairModel update mergeSecond msg ( model, modelChanged ) =
    update msg model
        |> Tuple.mapFirst (Tuple.mapSecond (mergeSecond modelChanged))


delegate : msg -> Cmd msg
delegate =
    Task.succeed >> Task.perform identity


withCmd : Cmd msg -> a -> ( a, Cmd msg )
withCmd cmd model =
    ( model, cmd )


withoutCmd : a -> ( a, Cmd msg )
withoutCmd =
    withCmd Cmd.none
