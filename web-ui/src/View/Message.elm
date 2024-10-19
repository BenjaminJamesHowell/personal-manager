module View.Message exposing
    ( parent
    , loading
    , httpError
    , queryError
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import PersonalListQuery
import State
import Http

parent : List (Html State.Msg) -> Html State.Msg
parent = div [ class "message" ]

loading : Html State.Msg
loading = parent [ text "Loading" ]

httpError : Http.Error -> Html State.Msg
httpError error =
    case error of
        Http.BadUrl e ->
            parent
                [ text "HTTP Error"
                , br [] []
                , text <| "Bad URL: " ++ e
                ]

        Http.Timeout ->
            parent
                [ text "HTTP Error"
                , br [] []
                , text "Timeout"
                ]

        Http.NetworkError ->
            parent
                [ text "HTTP Error"
                , br [] []
                , text "Network Error"
                ]

        Http.BadStatus status ->
            parent
                [ text "HTTP Error"
                , br [] []
                , text <| String.fromInt status
                ]

        Http.BadBody err ->
            parent
                [ text "JSON Parse error"
                , br [] []
                , text err
                ]

queryError : PersonalListQuery.Error -> Html State.Msg
queryError error =
    case error of
        PersonalListQuery.ListDoesNotExist ->
            parent
                [ text "Query Error"
                , br [] []
                , text "List does not exist"
                ]

