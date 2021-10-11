module Util.Update exposing (..)


chain : msg -> (msg -> a -> ( a, Cmd msg )) -> ( a, Cmd msg ) -> ( a, Cmd msg )
chain msg update ( model, cmd ) =
    update msg model
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])
