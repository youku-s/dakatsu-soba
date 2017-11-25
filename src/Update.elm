module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)
import Task exposing (perform)
import Window exposing (size)
import Html5.DragDrop as DragDrop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        AddRow newCmd -> (model, newCmd(model))
        RemoveRow f ->(f(model), Cmd.none)
        ToggleEdit tab -> (
            let
                newTabState = {tab | isEditing = not tab.isEditing}
                eq = \x -> x.uuid == tab.uuid
            in
                {model |
                    activeTab = OtherTab newTabState,
                    -- 表示をtoggleしたtab以外は編集モードを解除する
                    tabs = 
                        (List.map (\x -> {x | isEditing = False}) (Utils.takeNotWhile eq model.tabs)) ++ 
                        [newTabState] ++ 
                        (List.map (\x -> {x | isEditing = False}) (Utils.dropNotWhile eq model.tabs))
                }
            ,
            Cmd.none
        )
        PersonalTabClicked -> ({ model | activeTab = ProfileTab, tabs = List.map (\x -> {x | isEditing = False}) model.tabs }, Cmd.none)
        ClassesTabClicked -> ({ model | activeTab = ClassesTab, tabs = List.map (\x -> {x | isEditing = False}) model.tabs }, Cmd.none)
        FavorsTabClicked -> ({ model | activeTab = FavorsTab, tabs = List.map (\x -> {x | isEditing = False}) model.tabs }, Cmd.none)
        OtherTabClicked tab -> ({ model |
            activeTab = OtherTab tab,
            tabs = 
                let
                    eq = \x -> x.uuid == tab.uuid
                in
                    (List.map (\x -> {x | isEditing = False}) (Utils.takeNotWhile eq model.tabs)) ++ 
                    [tab] ++ 
                    (List.map (\x -> {x | isEditing = False}) (Utils.dropNotWhile eq model.tabs))
        }, Cmd.none)
        FormUpdated f ->(f(model), Cmd.none)
        AddTab newCmd -> (model, newCmd(model))
        RemoveTab tab -> 
            let
                eq = \x -> x.uuid == tab.uuid
                activeTabRemoved = 
                    case model.activeTab of
                        OtherTab activeTab -> activeTab.uuid == tab.uuid
                        _ -> False
            in                
                ({model |
                    tabs = (Utils.takeNotWhile eq model.tabs) ++ (Utils.dropNotWhile eq model.tabs),
                    activeTab = if activeTabRemoved then ProfileTab else model.activeTab
                }, Cmd.none)
        AddTag -> (
            {model | tagform = "", tags = ((List.filter (\x -> x /= model.tagform) model.tags) ++ [model.tagform])},
            Cmd.none
        )
        RemoveTag tagname -> (
            {model | tags = List.filter (\x -> x /= tagname) model.tags },
            Cmd.none
        )
        OpenDialog tab -> ({model | showDeleteTabialog = Just tab}, (Task.perform SetWindowSize Window.size))
        CloseDialog tab ->
            let
                uuid = case tab of
                    Just tb -> tb.uuid
                    Nothing -> ""

                eq = \x -> x.uuid == uuid

                activeTabRemoved =
                    case (model.activeTab, tab) of
                        (OtherTab activeTab, Just tab) -> activeTab.uuid == tab.uuid
                        _ -> False
                
                isRemoved = case tab of
                    Just tb -> True
                    Nothing -> False
            in                
                ({model |
                    tabs = if isRemoved then (Utils.takeNotWhile eq model.tabs) ++ (Utils.dropNotWhile eq model.tabs) else model.tabs,
                    activeTab = if activeTabRemoved then ProfileTab else model.activeTab,
                    showDeleteTabialog = Nothing
                }, Cmd.none)
        SetWindowSize sz -> ({model | windowSize = sz}, Cmd.none)
        RowDragDrop msg_ ->
            let
                tab =
                    case model.activeTab of
                        OtherTab tb -> Just tb
                        _ -> Nothing

                (model_, result) =
                    DragDrop.update msg_ model.dragDrop

                nextPos = \pos -> case pos of
                    Position idx -> Position (idx + 1)

                unwrap = \x ->
                    case x of
                        Position idx -> idx

                newTabState =
                    Maybe.map 
                        (\tb ->
                            {tb |
                                tabType = case (result, tb.tabType) of
                                    (Just (dragId, dropId), ManeuvaTab items) ->
                                        let
                                            item = List.head (List.filter (\x -> x.uuid == dragId) items) 
                                        in
                                            case item of
                                                Nothing -> tb.tabType
                                                Just itm ->
                                                    let
                                                        src = unwrap itm.position 

                                                        dst = unwrap dropId

                                                        targetMoved =
                                                            Utils.move src dst items

                                                        positionReNumbered =
                                                            Maybe.map (\ls -> List.map (\(i, x) -> {x | position = Position i}) (zipWithIndex ls)) targetMoved
                                                    in
                                                        case positionReNumbered of
                                                            Just is ->
                                                                ManeuvaTab (
                                                                    is
                                                                )
                                                            Nothing -> tb.tabType
                                    (Just (dragId, dropId), ResourceTab resources) ->
                                        let
                                            resource = List.head (List.filter (\x -> x.uuid == dragId) resources) 
                                        in
                                            case resource of
                                                Nothing -> tb.tabType
                                                Just res ->
                                                    let
                                                        src = unwrap res.position 

                                                        dst = unwrap dropId

                                                        targetMoved =
                                                            Utils.move src dst resources

                                                        positionReNumbered =
                                                            Maybe.map (\ls -> List.map (\(i, x) -> {x | position = Position i}) (zipWithIndex ls)) targetMoved
                                                    in
                                                        case positionReNumbered of
                                                            Just is ->
                                                                ResourceTab (
                                                                    is
                                                                )
                                                            Nothing -> tb.tabType
                                    _ -> tb.tabType
                            }
                        )
                        tab

                newTabs =
                    case tab of
                        Just tb ->
                            Just (
                                Utils.updateOnWayUseEq 
                                    model.tabs
                                    (\x -> x.uuid == tb.uuid)
                                    tb
                                    (\t -> case newTabState of
                                        Just newTab -> newTab
                                        Nothing -> t
                                    )
                            )
                        Nothing -> Nothing
            in
                (
                    case (result, newTabs, newTabState) of
                        (Just _, Just nts, Just nt) -> {model | activeTab = OtherTab nt, tabs = nts, dragDrop = model_}
                        _ -> {model | dragDrop = model_}
                    , Cmd.none
                )
                    


