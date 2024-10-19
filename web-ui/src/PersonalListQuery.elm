module PersonalListQuery exposing
    ( Response
    , Error(..)
    , Ok
    , Request
    , send
    )

import PersonalList
import Json.Decode
import Json.Encode
import Array exposing (Array)
import Http

-- TYPES

type alias Response = Result Error Ok

type Error
    = ListDoesNotExist

type alias Ok
    = Array (Int, PersonalList.Item)

type alias Request =
    { listName : String
    , nameContains : String
    , minPriorityLevel : PersonalList.PriorityLevel
    }

-- JSON

encoder : Request -> Json.Encode.Value
encoder request =
    Json.Encode.object
        [ ( "list_name", Json.Encode.string request.listName )
        , ( "name_contains", Json.Encode.string request.nameContains )
        , ( "min_priority_level", PersonalList.priorityLevelEncoder request.minPriorityLevel )
        ]

decoder : Json.Decode.Decoder Response
decoder =
    Json.Decode.oneOf
        [ okResponseDecoder |> Json.Decode.map Result.Ok
        , errorResponseDecoder |> Json.Decode.map Result.Err
        ]

errorResponseDecoder : Json.Decode.Decoder Error
errorResponseDecoder =
    Json.Decode.field "Err" errorTypeDecoder

errorTypeDecoder : Json.Decode.Decoder Error
errorTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen (\str ->
            case str of
                "ListDoesNotExist" ->
                    Json.Decode.succeed ListDoesNotExist

                _ ->
                    Json.Decode.fail <| "Cannot parse list error: " ++ str
        )

okResponseDecoder : Json.Decode.Decoder Ok
okResponseDecoder =
    Json.Decode.field "Ok"
        (Json.Decode.array okResponseItemDecoder)

okResponseItemDecoder : Json.Decode.Decoder (Int, PersonalList.Item)
okResponseItemDecoder =
    Json.Decode.map2 (\a -> \b -> (a, b))
        (Json.Decode.index 0 Json.Decode.int)
        (Json.Decode.index 1 PersonalList.itemDecoder)

-- HTTP

send : Request -> (Result Http.Error Response -> a) -> Cmd a
send request f =
    let
        url : String
        url = "http://localhost:8000/query"

        body : Http.Body
        body = Http.stringBody "application/json" (Json.Encode.encode 0 (encoder request))
    in
    Http.post
        { url = url
        , expect = Http.expectJson f decoder
        , body = body
        }

