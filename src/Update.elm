module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)
import Html5.DragDrop as DragDrop
import Regex
import String.Extra as SExtra
import Maybe.FlatMap
import List.FlatMap
import Http
import Json.Decode as Decode
import Json.Encode as Encode

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
        OpenDialog tab -> ({model | showDeleteTabialog = Just tab}, (requestDomSize "body"))
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
                    (requestDomSize "body")
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

                                escapeHokanjyo = \s -> s
                                    |> SExtra.replace "[" "<MLPAREN>"
                                    |> SExtra.replace "]" "<MRPAREN>"
                                    |> Regex.replace Regex.All (Regex.regex "<LLPAREN>([^<>]+)<LRPAREN>") (\match ->
                                        match.match
                                            |> SExtra.replace "：" "<ESCAPED_COLON>"
                                            |> SExtra.replace ":" "<ESCAPED_COLON>"
                                    )
                                    |> SExtra.replace ":" "<COLON>"
                                    |> SExtra.replace "｜" "<BAR>"
                                    |> SExtra.replace "|" "<BAR>"
                                    |> SExtra.replace "<ESCAPED_COLON>" "："

                                unescape = \s -> s
                                    |> SExtra.replace "<LPAREN>" "("
                                    |> SExtra.replace "<RPAREN>" ")"
                                    |> SExtra.replace "<LLPAREN>" "【"
                                    |> SExtra.replace "<LRPAREN>" "】"
                                    |> SExtra.replace "<MLPAREN>" "["
                                    |> SExtra.replace "<MRPAREN>" "]"
                                    |> SExtra.replace "<COLON>" "："
                                    |> SExtra.replace "<BAR>" "｜"
                                    
                                effect =
                                    Regex.regex "<LPAREN>(\\d+)<RPAREN>エフェクト<LLPAREN>([^<>]+)<LRPAREN>([^/<>]+)/([^<>]+)<COLON>([^<>]+)"

                                other =
                                    Regex.regex "<LPAREN>(\\d+)<RPAREN>([^<>]+)<BAR>([^<>]+)<LLPAREN>(.+?)<LRPAREN>([^/<>]+)/([^<>]+)<COLON>([^<>]+)"

                                hokanjyo =
                                    Regex.regex "<MLPAREN>([^<>]*)<MRPAREN>\\s*([^\\s]+?)\\s*<COLON>([^<>]*)<COLON>([^<>]*)<COLON>([^<>]*)<COLON>([^<>]*)"

                                proc = \s -> s 
                                    |> String.trim
                                    |> String.lines
                                    |> List.map 
                                        (\line ->
                                            let
                                                effectMatch = Regex.find Regex.All effect (escape line)
                                                otherMatch = Regex.find Regex.All other (escape line)
                                                hokanjyoMatch = Regex.find Regex.All hokanjyo (escapeHokanjyo line)

                                                effectMismatch = if List.isEmpty effectMatch then Just line else Nothing
                                                otherMismatch = if List.isEmpty otherMatch then Just line else Nothing
                                                hokanjyoMismatch = if List.isEmpty hokanjyoMatch then Just line else Nothing
                                                
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
                                                            name = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) name),
                                                            timing = AutoAlways,
                                                            cost = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) cost),
                                                            range = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) range),
                                                            description = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) description),
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
                                                        "ポジション" -> Skill
                                                        "メインクラス" -> Skill
                                                        "サブクラス" -> Skill
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
                                                            name = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) name),
                                                            timing = Maybe.withDefault AutoAlways (Maybe.map toTiming timing),
                                                            cost = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) cost),
                                                            range = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) range),
                                                            description = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) description),
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
                                                            maneuvaType = Maybe.withDefault Part (Maybe.map (\x -> x |> String.trim |> toManeuvaType2) region),
                                                            malice = Nothing,
                                                            favor = Nothing,
                                                            category = "0",
                                                            name = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) name),
                                                            timing = Maybe.withDefault AutoAlways (Maybe.map (\x -> x |> String.trim |> toTiming) timing),
                                                            cost = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) cost),
                                                            range = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) range),
                                                            description = Maybe.withDefault "" (Maybe.map (\x -> x |> String.trim |> unescape) description),
                                                            from = "",
                                                            region = Maybe.withDefault NoRegion (Maybe.map (\x -> x |> String.trim |> toRegion2) region),
                                                            position = Position 0
                                                        }
                                                    _ -> Nothing
                                            in
                                                if List.isEmpty effectMatch |> not then
                                                    (Nothing, List.FlatMap.flatMap (\match -> Maybe.withDefault [] (Maybe.map (\x -> [x]) (effectToManeuva match.submatches))) effectMatch)
                                                else if List.isEmpty otherMatch |> not then
                                                    (Nothing, List.FlatMap.flatMap (\match -> Maybe.withDefault [] (Maybe.map (\x -> [x]) (otherToManeuva match.submatches))) otherMatch)
                                                else if List.isEmpty hokanjyoMatch |> not then
                                                    (Nothing, List.FlatMap.flatMap (\match -> Maybe.withDefault [] (Maybe.map (\x -> [x]) (hokanjyoToManeuva match.submatches))) hokanjyoMatch)
                                                else
                                                    (hokanjyoMismatch, [])
                                        )
                                
                                matchResults =
                                    Maybe.withDefault [] (Maybe.map proc tabData.dialogContent)

                                newManeuvas = 
                                    List.FlatMap.flatMap (\(x, ls) -> ls) matchResults

                                mismatches = 
                                    List.FlatMap.flatMap
                                        (\(x, ls) -> case x of
                                            Just str -> [str]
                                            Nothing -> []
                                        ) matchResults
                            in    
                                {tab |
                                    mismatches = mismatches,
                                    tabType = ManeuvaTab {
                                        maneuvas = zipWithIndex (tabData.maneuvas ++ newManeuvas) |> List.map (\(i, x) -> {x | position = Position i}),
                                        showAddManeuvaDialog = False,
                                        dialogContent = Nothing
                                    }
                                }
                        _ -> tab
            in
                (
                    {model |
                        tabs = Utils.updateOnWay model.tabs tab (\tb -> newTabState),
                        activeTab = (OtherTab newTabState)
                    }
                        |> (\m ->
                            let
                                newModelState =
                                    List.foldr
                                        (\tab newM ->
                                            let
                                                (lastSeed, newTabType) =
                                                    case tab.tabType of
                                                        ManeuvaTab tabData -> 
                                                            let
                                                                (lastSeed, zipped) = Utils.zipWithUuid newM.seed (List.filter (\x -> String.isEmpty x.uuid) tabData.maneuvas)
                                                            in
                                                                (
                                                                    lastSeed,
                                                                    ManeuvaTab {
                                                                        tabData |
                                                                            maneuvas = (List.filter (\x -> not (String.isEmpty x.uuid)) tabData.maneuvas) ++ List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped
                                                                    }
                                                                )
                                                        _ -> (newM.seed, tab.tabType)
                                                
                                                newTabState = {tab | tabType = newTabType}
                                            in
                                                { newM |
                                                    seed = lastSeed,
                                                    activeTab = OtherTab newTabState,
                                                    tabs = Utils.updateOnWay newM.tabs tab (\x -> newTabState)
                                                }
                                        )
                                        m
                                        m.tabs
                            in
                                newModelState
                        ),
                    Cmd.none
                )

        ResetMessages tab ->
            let
                newTabState =
                    {tab | mismatches = []}

                newModelState =
                    {
                        model |
                            activeTab = OtherTab newTabState,
                            tabs = Utils.updateOnWay model.tabs tab (\x -> newTabState)
                    }
            in
                (newModelState, Cmd.none)
        
        UpdateSize size ->
            ({model | windowSize = size}, Cmd.none)

        Save ->
            let
                profileToValue p =
                    Encode.object
                        []

                favorToValue f =
                    Encode.object
                        []

                usedFavorToValue f =
                    Encode.object
                        []

                classesToValue c =
                    Encode.object
                        []

                tabToValue t =
                    Encode.object
                        []

                modelToValue m =
                    Encode.object
                        [
                            ("uuid", Encode.string m.uuid),
                            ("isPrivate", Encode.bool m.isPrivate),
                            ("password", Encode.string (Maybe.withDefault " " m.password)),
                            ("tags", Encode.list (List.map Encode.string m.tags)),
                            ("profile", profileToValue m.profile),
                            ("favors", Encode.list (List.map favorToValue m.favors)),
                            ("usedFavors", Encode.list (List.map usedFavorToValue m.usedFavors)),
                            ("classes", classesToValue m.classes),
                            ("tabs", Encode.list (List.map tabToValue m.tabs))
                        ]

                post url body =
                    Http.request
                        {
                            method = "POST",
                            headers = [
                                (Http.header "Content-Type" "application/json")
                            ],
                            url = url,
                            body = body,
                            expect = Http.expectStringResponse (\_ -> Ok ()),
                            timeout = Nothing,
                            withCredentials = False
                        }


                request =
                    post model.config.saveUrl (modelToValue model |> Http.jsonBody)

                -- TODO エラー処理
                onResponse request =
                    NoOp
            in
                (model, Http.send onResponse request)

        Delete ->
            (model, Cmd.none)

        Clone ->
            (model, Cmd.none)   