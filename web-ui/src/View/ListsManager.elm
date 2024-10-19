module View.ListsManager exposing
    ( viewListsManager
    )

import State
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import View.Message

viewListsManager : State.ListsManagerModel -> Html State.Msg
viewListsManager state =
    div
        [ class "lists-manager" ]
        <| viewListsManagerInner state

viewListsManagerInner : State.ListsManagerModel -> List (Html State.Msg)
viewListsManagerInner state =
    case state of
        State.ListsManagerLoading ->
            [ View.Message.loading
            ]

        State.ListsManagerHttpError httpError ->
            [ View.Message.httpError httpError
            ]

        State.ListsManagerOk names ->
            names
            |> List.map viewListName

viewListName : String -> Html State.Msg
viewListName name =
    button
        [ class "monospace no-border-top"
        , onClick <| State.SelectList name
        ]
        [ text name ]

