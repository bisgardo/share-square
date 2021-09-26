module Layout exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


containerClass : Html.Attribute msg
containerClass =
    class "container"


container : List (Html msg) -> Html msg
container =
    div [ containerClass ]


container1 : Html msg -> Html msg
container1 =
    List.singleton >> container


colClass : Html.Attribute msg
colClass =
    class "col"


col : List (Html msg) -> Html msg
col =
    div [ colClass ]


col1 : Html msg -> Html msg
col1 =
    List.singleton >> col


rowClass : Html.Attribute msg
rowClass =
    class "row"


row : List (Html msg) -> Html msg
row =
    div [ rowClass ]


row1 : Html msg -> Html msg
row1 =
    List.singleton >> row
