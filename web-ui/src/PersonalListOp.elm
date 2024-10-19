module PersonalListOp exposing
    ( Request(..)
    , CreateListOpRequestArgs
    , DeleteListOpRequestArgs
    , CreateListItemOpRequestArgs
    , ReplaceListItemOpRequestArgs
    , DeleteListItemOpRequestArgs

    , Response(..)
    , CreateListOpOk
    , CreateListOpError(..)
    , DeleteListOpOk
    , DeleteListOpError(..)
    , CreateListItemOpOk
    , CreateListItemOpError(..)
    , ReplaceListItemOpOk
    , ReplaceListItemOpError(..)
    , DeleteListItemOpOk
    , DeleteListItemOpError(..)

    , send
    )

import PersonalList
import Json.Decode
import Json.Encode
import Http

-- TYPES

type Request
    = CreateListOpRequest CreateListOpRequestArgs
    | DeleteListOpRequest DeleteListOpRequestArgs
    | CreateListItemOpRequest CreateListItemOpRequestArgs
    | ReplaceListItemOpRequest ReplaceListItemOpRequestArgs
    | DeleteListItemOpRequest DeleteListItemOpRequestArgs

type alias CreateListOpRequestArgs =
    { listName : String
    }

type alias DeleteListOpRequestArgs =
    { listName : String
    }

type alias CreateListItemOpRequestArgs =
    { listName : String
    , listItem : PersonalList.Item
    }

type alias ReplaceListItemOpRequestArgs =
    { listName : String
    , listItemIndex : Int
    , listItem : PersonalList.Item
    }

type alias DeleteListItemOpRequestArgs =
    { listName : String
    , listItemIndex : Int
    }

type Response
    = CreateListResponse (Result CreateListOpError CreateListOpOk)
    | DeleteListResponse (Result DeleteListOpError DeleteListOpOk)
    | CreateListItemResponse (Result CreateListItemOpError CreateListItemOpOk)
    | ReplaceListItemResponse (Result ReplaceListItemOpError ReplaceListItemOpOk)
    | DeleteListItemResponse (Result DeleteListItemOpError DeleteListItemOpOk)

type alias CreateListOpOk = {}
type CreateListOpError
    = CreateListErrorListAlreadyExists

type alias DeleteListOpOk = {}
type DeleteListOpError
    = DeleteListErrorListDoesNotExist

type alias CreateListItemOpOk =
    { listItemIndex : Int
    }
type CreateListItemOpError
    = CreateListItemErrorListDoesNotExist

type alias ReplaceListItemOpOk = {}
type ReplaceListItemOpError
    = ReplaceListItemErrorListDoesNotExist
    | ReplaceListItemErrorListItemDoesNotExist

type alias DeleteListItemOpOk = {}
type DeleteListItemOpError
    = DeleteListItemErrorListDoesNotExist
    | DeleteListItemErrorListItemDoesNotExist

-- JSON

encoder : Request -> Json.Encode.Value
encoder request =
    let
        args : List ( String, Json.Encode.Value )
        args = case request of
            CreateListOpRequest { listName } ->
                [ ( "list_name", Json.Encode.string listName )
                ]

            DeleteListOpRequest { listName } ->
                [ ( "list_name", Json.Encode.string listName )
                ]

            CreateListItemOpRequest { listName, listItem } ->
                [ ( "list_name", Json.Encode.string listName )
                , ( "list_item", PersonalList.itemEncoder listItem )
                ]

            ReplaceListItemOpRequest { listName, listItem, listItemIndex } ->
                [ ( "list_name", Json.Encode.string listName )
                , ( "list_item_index", Json.Encode.int listItemIndex )
                , ( "list_item", PersonalList.itemEncoder listItem )
                ]

            DeleteListItemOpRequest { listName, listItemIndex } ->
                [ ( "list_name", Json.Encode.string listName )
                , ( "list_item_index", Json.Encode.int listItemIndex )
                ]
    in
    Json.Encode.object
        [ ( "op", requestTypeEncoder request )
        , ( "args", Json.Encode.object args )
        ]

requestTypeEncoder : Request -> Json.Encode.Value
requestTypeEncoder request =
    case request of
        CreateListOpRequest _ ->
            Json.Encode.string "CreateList"

        DeleteListOpRequest _ ->
            Json.Encode.string "DeleteList"

        CreateListItemOpRequest _ ->
            Json.Encode.string "CreateListItem"

        ReplaceListItemOpRequest _ ->
            Json.Encode.string "ReplaceListItem"

        DeleteListItemOpRequest _ ->
            Json.Encode.string "DeleteListItem"

decoder : Json.Decode.Decoder Response
decoder =
    Json.Decode.oneOf
        [ createListOpDecoder
        , deleteListOpDecoder
        , createListItemOpDecoder
        , replaceListItemOpDecoder
        , deleteListItemOpDecoder
        ]

createListOpDecoder : Json.Decode.Decoder Response
createListOpDecoder =
    Json.Decode.map2 (\_ -> (\args -> CreateListResponse args))
        (Json.Decode.field "op" <| strDecoder "CreateList")
        (Json.Decode.field "args" createListOpArgsDecoder)

createListOpArgsDecoder : Json.Decode.Decoder (Result CreateListOpError CreateListOpOk)
createListOpArgsDecoder =
    Json.Decode.oneOf
        [ createListOpErrorDecoder 
        , createListOpOkDecoder
        ]

createListOpErrorDecoder : Json.Decode.Decoder (Result CreateListOpError CreateListOpOk)
createListOpErrorDecoder =
    Json.Decode.map Err
    <| Json.Decode.field "Err"
    <| createListErrorListAlreadyExistsDecoder

createListErrorListAlreadyExistsDecoder : Json.Decode.Decoder (CreateListOpError)
createListErrorListAlreadyExistsDecoder =
    strDecoder "ListAlreadyExists"
    |> Json.Decode.map (always CreateListErrorListAlreadyExists)

createListOpOkDecoder : Json.Decode.Decoder (Result CreateListOpError CreateListOpOk)
createListOpOkDecoder =
    Json.Decode.succeed <| Ok {}

deleteListOpDecoder : Json.Decode.Decoder Response
deleteListOpDecoder =
    Json.Decode.map2 (\_ -> (\args -> DeleteListResponse args))
        (Json.Decode.field "op" <| strDecoder "DeleteList")
        (Json.Decode.field "args" deleteListOpArgsDecoder)

deleteListOpArgsDecoder : Json.Decode.Decoder (Result DeleteListOpError DeleteListOpOk)
deleteListOpArgsDecoder =
    Json.Decode.oneOf
        [ deleteListOpErrorDecoder 
        , deleteListOpOkDecoder
        ]

deleteListOpErrorDecoder : Json.Decode.Decoder (Result DeleteListOpError DeleteListOpOk)
deleteListOpErrorDecoder =
    Json.Decode.map Err
    <| Json.Decode.field "Err"
    <| deleteListErrorListDoesNotExistDecoder

deleteListErrorListDoesNotExistDecoder : Json.Decode.Decoder (DeleteListOpError)
deleteListErrorListDoesNotExistDecoder =
    strDecoder "ListDoesNotExist"
    |> Json.Decode.map (always DeleteListErrorListDoesNotExist)

deleteListOpOkDecoder : Json.Decode.Decoder (Result DeleteListOpError DeleteListOpOk)
deleteListOpOkDecoder =
    Json.Decode.map Ok
    <| Json.Decode.field "Ok"
    <| Json.Decode.succeed {}

createListItemOpDecoder : Json.Decode.Decoder Response
createListItemOpDecoder =
    Json.Decode.map2 (\_ -> (\args -> CreateListItemResponse args))
        (Json.Decode.field "op" <| strDecoder "CreateListItem")
        (Json.Decode.field "args" createListItemOpArgsDecoder)

createListItemOpArgsDecoder : Json.Decode.Decoder (Result CreateListItemOpError CreateListItemOpOk)
createListItemOpArgsDecoder =
    Json.Decode.oneOf
        [ createListItemOpErrorDecoder 
        , createListItemOpOkDecoder
        ]

createListItemOpErrorDecoder : Json.Decode.Decoder (Result CreateListItemOpError CreateListItemOpOk)
createListItemOpErrorDecoder =
    Json.Decode.map Err
    <| Json.Decode.field "Err"
    <| createListItemErrorListDoesNotExistDecoder

createListItemErrorListDoesNotExistDecoder : Json.Decode.Decoder (CreateListItemOpError)
createListItemErrorListDoesNotExistDecoder =
    strDecoder "ListDoesNotExist"
    |> Json.Decode.map (always CreateListItemErrorListDoesNotExist)

createListItemOpOkDecoder : Json.Decode.Decoder (Result CreateListItemOpError CreateListItemOpOk)
createListItemOpOkDecoder =
    Json.Decode.map Ok
    <| Json.Decode.field "Ok"
    <| Json.Decode.map (\i -> { listItemIndex = i })
    <| Json.Decode.field "list_item_index" Json.Decode.int

replaceListItemOpDecoder : Json.Decode.Decoder Response
replaceListItemOpDecoder =
    Json.Decode.map2 (\_ -> (\args -> ReplaceListItemResponse args))
        (Json.Decode.field "op" <| strDecoder "ReplaceListItem")
        (Json.Decode.field "args" replaceListItemOpArgsDecoder)

replaceListItemOpArgsDecoder : Json.Decode.Decoder (Result ReplaceListItemOpError ReplaceListItemOpOk)
replaceListItemOpArgsDecoder =
    Json.Decode.oneOf
        [ replaceListItemOpErrorDecoder 
        , replaceListItemOpOkDecoder
        ]

replaceListItemOpErrorDecoder : Json.Decode.Decoder (Result ReplaceListItemOpError ReplaceListItemOpOk)
replaceListItemOpErrorDecoder =
    Json.Decode.map Err
    <| Json.Decode.field "Err"
    <| Json.Decode.oneOf
        [ replaceListItemErrorListDoesNotExistDecoder
        , replaceListItemErrorListItemDoesNotExistDecoder
        ]

replaceListItemErrorListDoesNotExistDecoder : Json.Decode.Decoder (ReplaceListItemOpError)
replaceListItemErrorListDoesNotExistDecoder =
    strDecoder "ListDoesNotExist"
    |> Json.Decode.map (always ReplaceListItemErrorListDoesNotExist)

replaceListItemErrorListItemDoesNotExistDecoder : Json.Decode.Decoder (ReplaceListItemOpError)
replaceListItemErrorListItemDoesNotExistDecoder =
    strDecoder "ListItemDoesNotExist"
    |> Json.Decode.map (always ReplaceListItemErrorListItemDoesNotExist)

replaceListItemOpOkDecoder : Json.Decode.Decoder (Result ReplaceListItemOpError ReplaceListItemOpOk)
replaceListItemOpOkDecoder =
    Json.Decode.map Ok
    <| Json.Decode.field "Ok"
    <| Json.Decode.succeed {}

deleteListItemOpDecoder : Json.Decode.Decoder Response
deleteListItemOpDecoder =
    Json.Decode.map2 (\_ -> (\args -> DeleteListItemResponse args))
        (Json.Decode.field "op" <| strDecoder "DeleteListItem")
        (Json.Decode.field "args" deleteListItemOpArgsDecoder)

deleteListItemOpArgsDecoder : Json.Decode.Decoder (Result DeleteListItemOpError DeleteListItemOpOk)
deleteListItemOpArgsDecoder =
    Json.Decode.oneOf
        [ deleteListItemOpErrorDecoder 
        , deleteListItemOpOkDecoder
        ]

deleteListItemOpErrorDecoder : Json.Decode.Decoder (Result DeleteListItemOpError DeleteListItemOpOk)
deleteListItemOpErrorDecoder =
    Json.Decode.map Err
    <| Json.Decode.field "Err"
    <| Json.Decode.oneOf
        [ deleteListItemErrorListDoesNotExistDecoder
        , deleteListItemErrorListItemDoesNotExistDecoder
        ]

deleteListItemErrorListDoesNotExistDecoder : Json.Decode.Decoder (DeleteListItemOpError)
deleteListItemErrorListDoesNotExistDecoder =
    strDecoder "ListDoesNotExist"
    |> Json.Decode.map (always DeleteListItemErrorListDoesNotExist)

deleteListItemErrorListItemDoesNotExistDecoder : Json.Decode.Decoder (DeleteListItemOpError)
deleteListItemErrorListItemDoesNotExistDecoder =
    strDecoder "ListItemDoesNotExist"
    |> Json.Decode.map (always DeleteListItemErrorListItemDoesNotExist)

deleteListItemOpOkDecoder : Json.Decode.Decoder (Result DeleteListItemOpError DeleteListItemOpOk)
deleteListItemOpOkDecoder =
    Json.Decode.map Ok
    <| Json.Decode.field "Ok"
    <| Json.Decode.succeed {}

strDecoder : String -> Json.Decode.Decoder ()
strDecoder search =
    Json.Decode.string
        |> Json.Decode.andThen (\str ->
            if str == search then
                Json.Decode.succeed ()
            else
                Json.Decode.fail <| "Expected an exact string"
        )

-- HTTP

send : Request -> (Result Http.Error Response -> a) -> Cmd a
send request f =
    let
        url : String
        url = "http://localhost:8000/op"

        body : Http.Body
        body = Http.stringBody "application/json"
            <| Json.Encode.encode 0 (encoder request)
    in
    Http.post
        { url = url
        , expect = Http.expectJson f decoder
        , body = body
        }

