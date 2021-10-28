port module Layout exposing (..)

import Html exposing (Html, button, div, h5, text)
import Html.Attributes exposing (class, disabled, tabindex, type_)
import Html.Events exposing (onCheck, onInput)
import Maybe.Extra as Maybe
import Set exposing (Set)


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


modal : String -> String -> List (Html msg) -> Bool -> Html msg
modal key title body disableSubmit =
    div [ Html.Attributes.id key, class "modal fade", tabindex -1 ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ]
                        [ text title
                        ]
                    , button [ type_ "button", class "btn-close", data "bs-dismiss" "modal" ]
                        []
                    ]
                , div [ class "modal-body" ] body
                , div [ class "modal-footer" ]
                    [ button [ type_ "button", class "btn btn-secondary", data "bs-dismiss" "modal" ] [ text "Close" ]
                    , button [ type_ "submit", class "btn btn-primary", disabled disableSubmit ] [ text "Save" ]
                    ]
                ]
            ]
        ]


openModalButton : String -> String -> String -> List (Html.Attribute msg) -> Html msg
openModalButton id key label attributes =
    button
        ([ Html.Attributes.id id
         , type_ "button"
         , class "btn btn-primary"
         , data "bs-toggle" "modal"
         , data "bs-target" ("#" ++ key)
         ]
            ++ attributes
        )
        [ text label ]


data : String -> String -> Html.Attribute msg
data key val =
    Html.Attributes.attribute ("data-" ++ key) val



-- FORM STUFF


type alias Field =
    { key : String
    , value : String
    }


type alias Validated a =
    { a
        | validationError : Maybe String
    }


isInvalid : Validated a -> Bool
isInvalid =
    .validationError >> Maybe.isJust


optionsInput : String -> String -> List Field -> String -> (String -> msg) -> Html msg
optionsInput key label options value tagger =
    div [ rowClass, class "row mb-3" ]
        [ Html.label [ class "col-sm-3 col-form-label", Html.Attributes.for key ] [ text label ]
        , div [ class "col-sm-9" ]
            [ Html.select
                [ Html.Attributes.id key, class "form-select", onInput tagger ]
                (List.map (optionInput value) options)
            ]
        ]


optionInput : String -> Field -> Html msg
optionInput value field =
    Html.option
        ([ Html.Attributes.value field.key ]
            ++ optionSelected (field.key == value)
        )
        [ text field.value ]


optionSelected : Bool -> List (Html.Attribute msg)
optionSelected condition =
    if condition then
        [ Html.Attributes.attribute "selected" "selected" ]

    else
        []


textInput : String -> Validated Field -> (String -> msg) -> Html msg
textInput label field tagger =
    div [ class "row mb-3" ]
        [ Html.label [ class "col-sm-3 col-form-label", Html.Attributes.for field.key ] [ text label ]
        , div [ class "col-sm-9" ] <|
            [ Html.input
                ([ Html.Attributes.id field.key
                 , type_ "text"
                 , class "form-control"
                 , onInput tagger
                 , Html.Attributes.value field.value
                 ]
                    ++ validation field.validationError (always <| class "is-invalid")
                )
                []
            ]
                ++ validation field.validationError
                    (\msg -> div [ class "invalid-feedback" ] [ text msg ])
        ]


checkboxesInput : String -> List Field -> Set String -> (String -> Bool -> msg) -> Html msg
checkboxesInput label fields checkedKeys tagger =
    Html.fieldset [ class "row mb-3" ]
        [ Html.legend [ class "col-form-label col-sm-3 pt-0" ] [ text label ]
        , div [ class "col-sm-9" ] <|
            List.map
                (\field ->
                    div [ class "form-check" ]
                        [ Html.input
                            [ type_ "checkbox"
                            , class "form-check-input"
                            , Html.Attributes.id field.key
                            , Html.Attributes.checked (Set.member field.key checkedKeys)
                            , onCheck (tagger field.key)
                            ]
                            []
                        , Html.label
                            [ class "form-check-label d-block" -- adding 'display: block' to make the label fill all available space
                            , Html.Attributes.for field.key
                            ]
                            [ text field.value ]
                        ]
                )
                fields
        ]


validation : Maybe String -> (String -> a) -> List a
validation error generate =
    Maybe.map generate error |> Maybe.toList


port closeModal : String -> Cmd msg


port modalClosed : (String -> msg) -> Sub msg


type Msg
    = ModalClosed String
