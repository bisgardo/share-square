module Util.Update exposing (..)

import Task


chain : msg -> (msg -> a -> ( b, Cmd msg )) -> ( a, Cmd msg ) -> ( b, Cmd msg )
chain msg update ( model, cmd ) =
    update msg model
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])


delegate : msg -> Cmd msg
delegate =
    Task.succeed >> Task.perform identity
