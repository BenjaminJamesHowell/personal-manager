module PersonalList exposing
    ( Item
    , PriorityLevel(..)
    , itemDecoder
    , itemEncoder
    , priorityLevelEncoder
    , getListNames
    )

import Http
import Json.Decode
import Json.Encode

-- TYPES

type alias Item =
    { name : String
    , priorityLevel : PriorityLevel
    }

type PriorityLevel
    = High
    | Med
    | Low

priorityLevelToString : PriorityLevel -> String
priorityLevelToString priorityLevel =
    case priorityLevel of
        High ->
            "High"

        Med ->
            "Med"

        Low ->
            "Low"

-- JSON

itemEncoder : Item -> Json.Encode.Value
itemEncoder { name, priorityLevel } =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "priority_level", priorityLevelEncoder priorityLevel )
        ]

priorityLevelEncoder : PriorityLevel -> Json.Encode.Value
priorityLevelEncoder p =
    Json.Encode.string <| priorityLevelToString p

itemDecoder : Json.Decode.Decoder Item
itemDecoder =
    Json.Decode.map2 Item
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "priority_level" priorityLevelDecoder)

priorityLevelDecoder : Json.Decode.Decoder PriorityLevel
priorityLevelDecoder =
    Json.Decode.string
        |> Json.Decode.andThen (\str ->
            case str of
                "High" ->
                    Json.Decode.succeed High

                "Med" ->
                    Json.Decode.succeed Med

                "Low" ->
                    Json.Decode.succeed Low

                _ ->
                    Json.Decode.fail <| "Cannot parse priority level: " ++ str
        )

listNamesDecoder : Json.Decode.Decoder (List String)
listNamesDecoder =
    Json.Decode.list Json.Decode.string

-- HTTP

getListNames : (Result Http.Error (List String) -> a) -> Cmd a
getListNames f =
    let
        url : String
        url = "http://localhost:8000/lists"
    in
    Http.get
        { url = url
        , expect = Http.expectJson f listNamesDecoder
        }

