module View.ListManager exposing
    ( viewListManager
    )

import State
import Array
import View.Message
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import PersonalList
import PersonalListQuery

viewListManager : State.ListManagerModel -> Html State.Msg
viewListManager state =
    div
        [ class "list-manager" ]
        <| viewListManagerInner state

viewListManagerInner : State.ListManagerModel -> List (Html State.Msg)
viewListManagerInner state =
    case state of
        State.ListManagerNoneSelected ->
            View.Message.parent [ text "No List Selected" ]
            |> List.singleton

        State.ListManagerLoading ->
            View.Message.loading
            |> List.singleton

        State.ListManagerHttpError httpError ->
            View.Message.httpError httpError
            |> List.singleton

        State.ListManagerQueryError queryError ->
            View.Message.queryError queryError
            |> List.singleton

        State.ListManagerOk request items ->
            [ viewListMetadata request <| Array.length items
            , viewListItems items
            ]

viewListMetadata : PersonalListQuery.Request -> Int -> Html State.Msg
viewListMetadata request length =
    div
        [ class "list-metadata" ]
        [ h1 [] [ text request.listName ]
        , text <| "Contains " ++ (String.fromInt length) ++ " items"
        ]

viewListItems : PersonalListQuery.Ok -> Html State.Msg
viewListItems items =
    let
        headings : List (Html State.Msg) 
        headings =
            [ th [ class "index-heading" ] [ text "Index" ]
            , th [ class "priority-level-heading" ] [ text "Priority Level" ]
            , th [ class "name-heading" ] [ text "Name" ]
            ]

        rows : List (Html State.Msg) 
        rows =
            items
            |> Array.map viewListItem
            |> Array.toList

    in
    table
        [ class "list-items" ]
        <| headings ++ rows

viewListItem : ( Int, PersonalList.Item ) -> Html State.Msg
viewListItem ( i, item ) =
    tr
        []
        [ td [] [ text <| String.fromInt i ]
        , viewPriorityLevel ( i, item )
        , viewListName ( i, item )
        ]

viewPriorityLevel : ( Int, PersonalList.Item ) -> Html State.Msg
viewPriorityLevel ( i, item ) =
    td
        [ class "priority-level-cell"
        ]
        [ select
            [ onInput <| State.UpdateItemPriorityLevel i
            ]
            [ option
                [ value "High"
                , selected <| item.priorityLevel == PersonalList.High
                ]
                [ text "High" ]
            , option
                [ value "Med"
                , selected <| item.priorityLevel == PersonalList.Med
                ]
                [ text "Med" ]
            , option
                [ value "Low"
                , selected <| item.priorityLevel == PersonalList.Low
                ]
                [ text "Low" ]
            ]
        ]

viewListName : ( Int, PersonalList.Item ) -> Html State.Msg
viewListName ( i, item ) =
    td
        []
        [ textarea
            [ class "item-name"
            , value item.name
            , onInput <| State.UpdateItemName i
            ]
            []
        ]

