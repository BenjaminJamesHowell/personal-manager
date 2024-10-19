module View exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import State
import View.ListsManager
import View.ListManager

view : State.Model -> Html State.Msg
view model =
    div
        [ class "app" ]
        [ View.ListsManager.viewListsManager model.listsManager
        , View.ListManager.viewListManager model.listManager
        ]

