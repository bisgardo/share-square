module Util.Update exposing (..)

import Task


chain : (msg -> a -> ( b, Cmd msg )) -> msg -> ( a, Cmd msg ) -> ( b, Cmd msg )
chain update msg ( model, cmd ) =
    update msg model
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])

chains : (msg -> a -> ( a, Cmd msg )) -> List msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
chains update msgs modelCmdPair = msgs |> List.foldl (chain update) modelCmdPair

delegate : msg -> Cmd msg
delegate =
    Task.succeed >> Task.perform identity


withCmd : Cmd msg -> a -> ( a, Cmd msg )
withCmd cmd model =
    ( model, cmd )


withoutCmd : a -> ( a, Cmd msg )
withoutCmd =
    withCmd Cmd.none
