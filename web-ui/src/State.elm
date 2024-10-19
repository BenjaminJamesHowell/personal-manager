module State exposing
    ( Model
    , ListsManagerModel(..)
    , ListManagerModel(..)

    , init
    , subscriptions

    , Msg(..)
    , update
    )

import PersonalList
import PersonalListQuery
import PersonalListOp
import Http
import Array

-- TYPES

type alias Model =
    { listsManager : ListsManagerModel
    , listManager : ListManagerModel
    }

type ListsManagerModel
    = ListsManagerLoading
    | ListsManagerHttpError Http.Error
    | ListsManagerOk (List String)

type ListManagerModel
    = ListManagerNoneSelected
    | ListManagerLoading
    | ListManagerHttpError Http.Error
    | ListManagerQueryError PersonalListQuery.Error
-- The Request here stores the filters that are currently being used in the list
-- manager ("what list is being shown?", "what priority is being shown?").
--
-- The Ok here DOES NOT necessarily reflect the actual server state, just the
-- state being shown to the user. When the user changes the list state, the
-- state saved in here is changed AND a ListOpRequest is sent to the server.
-- This saves me from re-requesting server state every ListOp.
    | ListManagerOk PersonalListQuery.Request PersonalListQuery.Ok

-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { listsManager = initListsManager
            , listManager = initListManager
            }

        loadListNamesCommand : Cmd Msg
        loadListNamesCommand =
            PersonalList.getListNames LoadListNamesComplete
    in
    ( model, loadListNamesCommand )

initListsManager : ListsManagerModel
initListsManager = ListsManagerLoading

initListManager : ListManagerModel
initListManager = ListManagerNoneSelected

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATE

type Msg
    -- Commands
    = LoadListNamesComplete (Result Http.Error (List String))
    | LoadListComplete PersonalListQuery.Request (Result Http.Error PersonalListQuery.Response)
    | NoopMsg

    -- HTML Events
    | SelectList String
    | UpdateItemPriorityLevel Int String
    | UpdateItemName Int String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadListNamesComplete result ->
            ( { model | listsManager = updateLoadListNamesComplete result }
            , Cmd.none
            )

        LoadListComplete request result ->
            ( { model | listManager = updateLoadListComplete request result }
            , Cmd.none
            )

        NoopMsg -> ( model, Cmd.none )

        SelectList name ->
            let
                ( listManager, command ) = updateSelectList name
            in
            ( { model | listManager = listManager }
            , command
            )

        UpdateItemPriorityLevel i priorityLevelStr ->
            let
                ( listManager, command ) = updateItemPriorityLevel i priorityLevelStr model.listManager
            in
            ( { model | listManager = listManager }
            , command
            )

        UpdateItemName i newName ->
            let
                ( listManager, command ) = updateItemName i newName model.listManager
            in
            ( { model | listManager = listManager }
            , command
            )

updateLoadListNamesComplete : (Result Http.Error (List String)) -> ListsManagerModel
updateLoadListNamesComplete result =
    case result of
        Err httpError ->
            ListsManagerHttpError httpError
        
        Ok listNames ->
            ListsManagerOk listNames

updateLoadListComplete : PersonalListQuery.Request -> (Result Http.Error PersonalListQuery.Response) -> ListManagerModel
updateLoadListComplete request result =
    case result of
        Err httpError ->
            ListManagerHttpError httpError

        Ok (Err queryError) ->
            ListManagerQueryError queryError

        Ok (Ok items) ->
            ListManagerOk request items

updateSelectList : String -> ( ListManagerModel, Cmd Msg )
updateSelectList name =
    let
        request : PersonalListQuery.Request
        request =
            { listName = name
            , nameContains = ""
            , minPriorityLevel = PersonalList.Low
            }

        model : ListManagerModel
        model = ListManagerLoading

        cmd : Cmd Msg
        cmd = PersonalListQuery.send request <| LoadListComplete request
    in
    ( model, cmd )

updateItemPriorityLevel : Int -> String -> ListManagerModel -> ( ListManagerModel, Cmd Msg )
updateItemPriorityLevel i priorityLevelStr state =
    case state of
        ListManagerOk request items ->
            case Array.get i items of
                Nothing ->
                    ( state, Cmd.none )

                Just (_, oldItem) ->
                    let 
                        newPriorityLevel : PersonalList.PriorityLevel
                        newPriorityLevel =
                            case priorityLevelStr of
                                "High" ->
                                    PersonalList.High

                                "Med" ->
                                    PersonalList.Med

                                "Low" ->
                                    PersonalList.Low

                                _ ->
                                    PersonalList.Low

                        newItem : PersonalList.Item
                        newItem = { oldItem | priorityLevel = newPriorityLevel }

                        newItems : PersonalListQuery.Ok
                        newItems = Array.set i (i, newItem) items

                        opRequest : PersonalListOp.Request
                        opRequest =
                            PersonalListOp.ReplaceListItemOpRequest
                                { listName = request.listName
                                , listItemIndex = i
                                , listItem = newItem
                                }
                    in
                    ( ListManagerOk request newItems
                    , PersonalListOp.send opRequest <| always NoopMsg
                    )

        _ ->
            ( state, Cmd.none )

updateItemName : Int -> String -> ListManagerModel -> ( ListManagerModel, Cmd Msg )
updateItemName i newName state =
    case state of
        ListManagerOk request items ->
            case Array.get i items of
                Nothing ->
                    ( state, Cmd.none )

                Just ( _, oldItem ) ->
                    let
                        newItem : PersonalList.Item
                        newItem = { oldItem | name = newName }

                        newItems : PersonalListQuery.Ok
                        newItems = Array.set i ( i, newItem ) items
        
                        opRequest : PersonalListOp.Request
                        opRequest =
                            PersonalListOp.ReplaceListItemOpRequest
                                { listName = request.listName
                                , listItemIndex = i
                                , listItem = newItem
                                }
                    in
                    ( ListManagerOk request newItems
                    , PersonalListOp.send opRequest <| always NoopMsg
                    )

        _ ->
            ( state, Cmd.none )

