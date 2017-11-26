module Update exposing (..)

import Random.Pcg exposing (generate)
import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)
import Task exposing (perform)
import Window exposing (size)
import Html5.DragDrop as DragDrop
import Regex
import String.Extra as SExtra
import Maybe.FlatMap
import List.FlatMap
import Uuid.Barebones exposing (uuidStringGenerator)

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
                                    (Just (dragId, dropId), ManeuvaTab {maneuvas, showAddManeuvaDialog}) ->
                                        let
                                            item = List.head (List.filter (\x -> x.uuid == dragId) maneuvas) 
                                        in
                                            case item of
                                                Nothing -> tb.tabType
                                                Just itm ->
                                                    let
                                                        src = unwrap itm.position 

                                                        dst = unwrap dropId

                                                        targetMoved =
                                                            Utils.move src dst maneuvas

                                                        positionReNumbered =
                                                            Maybe.map (\ls -> List.map (\(i, x) -> {x | position = Position i}) (zipWithIndex ls)) targetMoved
                                                    in
                                                        case positionReNumbered of
                                                            Just is ->
                                                                ManeuvaTab {
                                                                    dialogContent = Nothing,
                                                                    maneuvas = is,
                                                                    showAddManeuvaDialog = False
                                                                }
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
        OpenManeuvaDialog tab ->
            let
                newTabState =
                    case tab.tabType of
                        ManeuvaTab tabData ->    
                            {tab |
                                tabType = ManeuvaTab {tabData | showAddManeuvaDialog = True}
                            }
                        _ -> tab
            in    
                (
                    {model |
                        tabs = Utils.updateOnWay model.tabs tab (\tb -> newTabState),
                        activeTab = OtherTab newTabState
                    },
                    (Task.perform SetWindowSize Window.size)
                )
        CloseManeuvaDialog tab ->
            let
                newTabState =
                    case tab.tabType of
                        ManeuvaTab tabData ->
                            let
                                -- https://github.com/elm-lang/elm-compiler/issues/1618 対策
                                escape = \s -> s
                                    |> SExtra.replace "(" "<LPAREN>"
                                    |> SExtra.replace ")" "<RPAREN>"
                                    |> SExtra.replace "[" "<MLPAREN>"
                                    |> SExtra.replace "]" "<MRPAREN>"
                                    |> SExtra.replace "【" "<LLPAREN>"
                                    |> SExtra.replace "】" "<LRPAREN>"
                                    |> Regex.replace Regex.All (Regex.regex "<LLPAREN>([^<>]+)<LRPAREN>") (\match ->
                                        match.match
                                            |> SExtra.replace "：" "<ESCAPED_COLON>"
                                            |> SExtra.replace ":" "<ESCAPED_COLON>"
                                    )
                                    |> SExtra.replace ":" "<COLON>"
                                    |> SExtra.replace "：" "<COLON>"
                                    |> SExtra.replace "｜" "<BAR>"
                                    |> SExtra.replace "|" "<BAR>"
                                    |> SExtra.replace " " ""
                                    |> SExtra.replace "　" ""
                                    |> SExtra.replace "<ESCAPED_COLON>" "："

                                unescape = \s -> s
                                    |> SExtra.replace "<LPAREN>" "("
                                    |> SExtra.replace "<RPAREN>" ")"
                                    |> SExtra.replace "<LLPAREN>" "【"
                                    |> SExtra.replace "<LRPAREN>" "】"
                                    |> SExtra.replace "<COLON>" "："
                                    |> SExtra.replace "<BAR>" "｜"
                                    
                                effect =
                                    Regex.regex "<LPAREN>(\\d+)<RPAREN>エフェクト<LLPAREN>([^<>]+)<LRPAREN>([^/<>]+)/([^<>]+)<COLON>([^<>]+)"

                                other =
                                    Regex.regex "<LPAREN>(\\d+)<RPAREN>([^<>]+)<BAR>([^<>]+)<LLPAREN>([^<>]+)<LRPAREN>([^/<>]+)/([^<>]+)<COLON>([^<>]+)"

                                hokanjyo =
                                -- [脚]　　　　　 あし　　　　　　　　　　　　　　　　　　　: ジャッジ　 : 1　　　: 0　 : 妨害１
                                    Regex.regex "<MLPAREN>([^<>]*)<MRPAREN>([^<>]*)<COLON>([^<>]*)<COLON>([^<>]*)<COLON>([^<>]*)<COLON>([^<>]*)"

                                proc = \s -> s 
                                    |> String.trim
                                    |> String.lines
                                    |> List.FlatMap.flatMap 
                                        (\line ->
                                            let
                                                effectMatch = Regex.find Regex.All effect (escape line)
                                                otherMatch = Regex.find Regex.All other (escape line)
                                                hokanjyoMatch = Regex.find Regex.All hokanjyo (escape line)

                                                strToInt = \s -> case String.toInt s of
                                                    Ok num -> Just num
                                                    _ -> Nothing

                                                effectToManeuva = \submatches -> case submatches of
                                                    malice :: name :: cost :: range :: description :: [] ->
                                                        Just {
                                                            uuid = "", 
                                                            used = False,
                                                            lost = False,
                                                            act = Nothing,
                                                            maneuvaType = Effect,
                                                            malice = Maybe.FlatMap.flatMap strToInt (Maybe.map String.trim malice),
                                                            favor = Nothing,
                                                            category = "0",
                                                            name = Maybe.withDefault "" (Maybe.map String.trim name),
                                                            timing = AutoAlways,
                                                            cost = Maybe.withDefault "" (Maybe.map String.trim cost),
                                                            range = Maybe.withDefault "" (Maybe.map String.trim range),
                                                            description = Maybe.withDefault "" (Maybe.map String.trim description),
                                                            from = "",
                                                            region = NoRegion,
                                                            position = Position 0
                                                        }
                                                    _ -> Nothing

                                                toTiming s =
                                                    case String.trim s of
                                                        "オート" -> AutoAlways
                                                        "アクション" -> Action
                                                        "ジャッジ" -> Judge
                                                        "ダメージ" -> Damage
                                                        "ラピッド" -> Rapid
                                                        "ラピット" -> Rapid -- 打ち間違え対策
                                                        _ -> AutoAlways

                                                toManeuvaType s =
                                                    case String.trim s of
                                                        "アーカイブ" -> Archive
                                                        _ -> Part

                                                toRegion s =
                                                    case String.trim s of
                                                        "なし" -> NoRegion 
                                                        "頭" -> Head
                                                        "腕" -> Arm
                                                        "胴" -> Body
                                                        "脚" -> Leg
                                                        "足" -> Leg -- 打ち間違え対策
                                                        _ -> OtherRegion

                                                toRegion2 s =
                                                    case String.trim s of
                                                        "なし" -> NoRegion 
                                                        "頭" -> Head
                                                        "腕" -> Arm
                                                        "胴" -> Body
                                                        "脚" -> Leg
                                                        "足" -> Leg -- 打ち間違え対策
                                                        _ -> NoRegion

                                                toManeuvaType2 s =
                                                    case String.trim s of
                                                        "なし" -> Part 
                                                        "頭" -> Part
                                                        "腕" -> Part
                                                        "胴" -> Part
                                                        "脚" -> Part
                                                        "足" -> Part -- 打ち間違え対策
                                                        _ -> Skill

                                                otherToManeuva = \submatches -> case submatches of
                                                    malice :: region :: timing :: name :: cost :: range :: description :: [] ->
                                                        Just {
                                                            uuid = "", 
                                                            used = False,
                                                            lost = False,
                                                            act = Nothing,
                                                            maneuvaType = Maybe.withDefault Part (Maybe.map toManeuvaType timing),
                                                            malice = Maybe.FlatMap.flatMap strToInt (Maybe.map String.trim malice),
                                                            favor = Nothing,
                                                            category = "0",
                                                            name = Maybe.withDefault "" (Maybe.map String.trim name),
                                                            timing = Maybe.withDefault AutoAlways (Maybe.map toTiming timing),
                                                            cost = Maybe.withDefault "" (Maybe.map String.trim cost),
                                                            range = Maybe.withDefault "" (Maybe.map String.trim range),
                                                            description = Maybe.withDefault "" (Maybe.map String.trim description),
                                                            from = "",
                                                            region = Maybe.withDefault NoRegion (Maybe.map toRegion region),
                                                            position = Position 0
                                                        }
                                                    _ -> Nothing
                                    
                                                hokanjyoToManeuva = \submatches -> case submatches of
                                                    region :: name :: timing :: cost :: range :: description :: [] ->
                                                        Just {
                                                            uuid = "", 
                                                            used = False,
                                                            lost = False,
                                                            act = Nothing,
                                                            maneuvaType = Maybe.withDefault Part (Maybe.map toManeuvaType2 timing),
                                                            malice = Nothing,
                                                            favor = Nothing,
                                                            category = "0",
                                                            name = Maybe.withDefault "" (Maybe.map String.trim name),
                                                            timing = Maybe.withDefault AutoAlways (Maybe.map toTiming timing),
                                                            cost = Maybe.withDefault "" (Maybe.map String.trim cost),
                                                            range = Maybe.withDefault "" (Maybe.map String.trim range),
                                                            description = Maybe.withDefault "" (Maybe.map String.trim description),
                                                            from = "",
                                                            region = Maybe.withDefault NoRegion (Maybe.map toRegion2 region),
                                                            position = Position 0
                                                        }
                                                    _ -> Nothing
                                            in
                                                if List.isEmpty effectMatch |> not then
                                                    List.FlatMap.flatMap (\match -> Maybe.withDefault [] (Maybe.map (\x -> [x]) (effectToManeuva match.submatches))) effectMatch
                                                else if List.isEmpty otherMatch |> not then
                                                    List.FlatMap.flatMap (\match -> Maybe.withDefault [] (Maybe.map (\x -> [x]) (otherToManeuva match.submatches))) otherMatch
                                                else if List.isEmpty hokanjyoMatch |> not then
                                                    List.FlatMap.flatMap (\match -> Maybe.withDefault [] (Maybe.map (\x -> [x]) (hokanjyoToManeuva match.submatches))) hokanjyoMatch
                                                else
                                                    []
                                        )

                                newManeuvas = 
                                    Maybe.withDefault [] (Maybe.map proc tabData.dialogContent)
                                
                            in    
                                {tab |
                                    tabType = ManeuvaTab {
                                        maneuvas = zipWithIndex (tabData.maneuvas ++ newManeuvas) |> List.map (\(i, x) -> {x | position = Position i}),
                                        showAddManeuvaDialog = False,
                                        dialogContent = Nothing
                                    }
                                }
                        _ -> tab
                
                newModelState =
                    {model |
                        tabs = Utils.updateOnWay model.tabs tab (\tb -> newTabState),
                        activeTab = (OtherTab newTabState)
                    }
            in
                (newModelState
                    ,(
                    Cmd.batch (
                        List.map
                            (\maneuva -> 
                                generate (\uuid -> FormUpdated (\m -> 
                                        let
                                            currentTab = 
                                                List.filter (\x -> x.uuid == tab.uuid) m.tabs
                                                    |> List.head
                                                    |> Maybe.withDefault tab
                                            
                                            newTabState2 = {currentTab | tabType = case currentTab.tabType of
                                                ManeuvaTab tabData -> ManeuvaTab {
                                                    tabData |
                                                        maneuvas = Utils.updateOnWayUseEq tabData.maneuvas (\x -> x == maneuva) maneuva (\x -> {x | uuid = uuid})
                                                }
                                                _ -> tab.tabType
                                            }
                                        in
                                            {m | 
                                                activeTab = OtherTab newTabState2,
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState2)
                                            }
                                    )
                                ) uuidStringGenerator
                            )
                            (case newTabState.tabType of
                                ManeuvaTab tabData -> List.filter (\x -> String.isEmpty x.uuid) tabData.maneuvas
                                _ -> []
                            )
                        )
                    )
                )
