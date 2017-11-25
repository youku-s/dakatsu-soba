module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)

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
        OpenDialog tab -> ({model | showDeleteTabialog = Just tab}, Cmd.none)
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
