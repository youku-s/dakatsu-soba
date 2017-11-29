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
import Routing exposing (parseLocation)

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
                -- Dynamoは空文字列エントリを許してくれないので、空文字列をnullに変換する必要がある
                strToValue s =
                    if s == "" then Encode.null else Encode.string s

                intToValue i =
                    if i == 0 then Encode.null else Encode.int i

                placeToString p =
                    case p of
                        Purgatory -> "Purgatory"
                        Garden -> "Garden"
                        Paradise -> "Paradise"

                memoryToValue me =
                    Encode.object  
                        [
                            ("uuid", me.uuid |> strToValue),
                            ("name", me.name |> strToValue),
                            ("description", me.description |> strToValue)
                        ]

                regretToValue r =
                    Encode.object
                        [
                            ("uuid", r.uuid |> strToValue),
                            ("target", r.target |> strToValue),
                            ("name", r.name |> strToValue),
                            ("currentVal", r.currentVal |> Encode.int),
                            ("maxVal", r.maxVal |> Encode.int),
                            ("negative", r.negative |> strToValue),
                            ("description", r.description |> strToValue)
                        ]
                
                karmaToString k =
                    Encode.object
                        [
                            ("uuid", k.uuid |> strToValue),
                            ("achieved", k.achieved |> Encode.bool),
                            ("name", k.name |> strToValue),
                            ("description", k.description |> strToValue)
                        ]

                profileToValue p =
                    Encode.object
                        [
                            ("name", p.name |> strToValue),
                            ("race", p.race |> strToValue),
                            ("age", p.age |> strToValue),
                            ("place", p.place |> placeToString |> Encode.string),
                            ("height", p.height |> strToValue),
                            ("weight", p.weight |> strToValue),
                            ("implication", p.implication |> strToValue),
                            ("hair", p.hair |> strToValue),
                            ("eye", p.eye |> strToValue),
                            ("skin", p.skin |> strToValue),
                            ("memo", p.memo |> strToValue),
                            ("memories", Encode.list (List.map memoryToValue p.memories)),
                            ("regrets", Encode.list (List.map regretToValue p.regrets)),
                            ("karmas", Encode.list (List.map karmaToString p.karmas))
                        ]

                favorToValue f =
                    Encode.object
                        [
                            ("uuid", f.uuid |> strToValue),
                            ("personal", (Maybe.withDefault 0 f.personal) |> intToValue),
                            ("battle", (Maybe.withDefault 0 f.battle) |> intToValue),
                            ("memo", f.memo |> strToValue)
                        ]

                usedFavorToValue f =
                    Encode.object
                        [
                            ("uuid", f.uuid |> strToValue),
                            ("purpose", f.purpose |> strToValue),
                            ("favor", f.favor |> Encode.int),
                            ("memo", f.memo |> strToValue)
                        ]

                positionToValue p =
                    Encode.object
                        [
                            ("uuid", p.uuid |> strToValue),
                            ("name", p.name |> strToValue)
                        ]

                subPositionToValue p =
                    Encode.object
                        [
                            ("uuid", p.uuid |> strToValue),
                            ("name", p.name |> strToValue)
                        ]

                highTechToValue h =
                    Encode.object
                        [
                            ("uuid", h.uuid |> strToValue),
                            ("name", h.name |> strToValue),
                            ("favor", (Maybe.withDefault 0 h.favor) |> intToValue)
                        ]

                categoryToString c =
                    case c of
                        MainClass -> "MainClass"
                        SubClass -> "SubClass"
                        SecondClass -> "SecondClass"
                        ThirdClass -> "ThirdClass"
                        ThirdPointFiveClass -> "ThirdPointFiveClass"
                        HighSociety -> "HighSociety"
                        OtherClass -> "OtherClass"

                classToValue c =
                    Encode.object
                        [
                            ("uuid", c.uuid |> strToValue),
                            ("category", c.category |> categoryToString |> Encode.string),
                            ("from", c.from |> strToValue),
                            ("name", c.name |> strToValue),
                            ("number", c.number |> intToValue)
                        ]

                pointToValue p =
                    Encode.object
                        [
                            ("uuid", p.uuid |> strToValue),
                            ("name", p.name |> strToValue),
                            ("busou", (Maybe.withDefault 0 p.busou) |> intToValue),
                            ("heni", (Maybe.withDefault 0 p.heni) |> intToValue),
                            ("kaizou", (Maybe.withDefault 0 p.heni) |> intToValue),
                            ("favor", (Maybe.withDefault 0 p.favor) |> intToValue)
                        ]

                classesToValue c =
                    Encode.object
                        [
                            ("positions", Encode.list (List.map positionToValue c.positions)),
                            ("subPositions", Encode.list (List.map subPositionToValue c.subPositions)),
                            ("highTechs", Encode.list (List.map highTechToValue c.highTechs)),
                            ("classes", Encode.list (List.map classToValue c.classes)),
                            ("points", Encode.list (List.map pointToValue c.points))
                        ]
                    
                maneuvaTypeToStr ma =
                    case ma of
                        Skill -> "Skill"
                        Part -> "Part"
                        Item -> "Item"
                        Effect -> "Effect"
                        Archive -> "Archive"
                        Tag -> "Tag"

                regionToStr re =
                    case re of
                        NoRegion -> "NoRegion"
                        Head -> "Head"
                        Arm -> "Arm"
                        Body -> "Body"
                        Leg -> "Leg"
                        OtherRegion -> "OtherRegion"

                timingToStr ti =
                    case ti of
                        AutoAlways -> "AutoAlways"
                        AutoNeedsDeclearation -> "AutoNeedsDeclearation"
                        AutoOthers -> "AutoOthers"
                        Action -> "Action"
                        Judge -> "Judge"
                        Damage -> "Damage"
                        Rapid -> "Rapid"
                        BeforeBattle -> "BeforeBattle"
                        BattleStart -> "BattleStart"
                        TurnStart -> "TurnStart"
                        CountStart -> "CountStart"

                posToValue p =
                    case p of
                        Position num -> num |> Encode.int

                maneuvaToValue ma =
                    Encode.object
                        [
                            ("uuid", ma.uuid |> strToValue),
                            ("used", ma.used |> Encode.bool),
                            ("lost", ma.lost |> Encode.bool),
                            ("act", (Maybe.withDefault 0 ma.act) |> intToValue),
                            ("malice", (Maybe.withDefault 0 ma.malice) |> intToValue),
                            ("favor", (Maybe.withDefault 0 ma.favor) |> intToValue),
                            ("maneuvaType", ma.maneuvaType |> maneuvaTypeToStr |> Encode.string),
                            ("category", ma.category |> strToValue),
                            ("name", ma.name |> strToValue),
                            ("timing", ma.timing |> timingToStr |> Encode.string),
                            ("cost", ma.cost |> strToValue),
                            ("range", ma.range |> strToValue),
                            ("description", ma.description |> strToValue),
                            ("from", ma.from |> strToValue),
                            ("region", ma.region |> regionToStr |> Encode.string),
                            ("position", ma.position |> posToValue)
                        ]

                resourceToValue re =
                    Encode.object
                        [
                            ("uuid", re.uuid |> strToValue),
                            ("name", re.name |> strToValue),
                            ("description", re.description |> strToValue),
                            ("favor", (Maybe.withDefault 0 re.favor) |> intToValue),
                            ("position", re.position |> posToValue)
                        ]

                tabTypeToItems tt =
                    case tt of
                        ManeuvaTab data -> Encode.list (List.map maneuvaToValue data.maneuvas)
                        ResourceTab items -> Encode.list (List.map resourceToValue items)

                tabTypeToStr t =
                    case t of
                        ManeuvaTab _ -> "ManeuvaTab"
                        ResourceTab _ -> "ResourceTab"

                tabToValue t =
                    Encode.object
                        [
                            ("uuid", t.uuid |> strToValue),
                            ("title", t.title |> strToValue),
                            ("tabType", t.tabType |> tabTypeToStr |> Encode.string),
                            ("items", t.tabType |> tabTypeToItems)
                        ]

                modelToValue m =
                    Encode.object
                        [
                            ("uuid", Encode.string m.uuid),
                            ("isPrivate", Encode.bool m.isPrivate),
                            ("password", (Maybe.withDefault "" m.password) |> strToValue),
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
                    post model.config.saveUrl (model |> modelToValue |> Http.jsonBody)

                -- TODO エラー処理
                onResponse request =
                    NoOp
            in
                (model, Http.send onResponse request)

        Delete ->
            -- TODO 実装する
            (model, Cmd.none)

        Clone ->
            -- TODO 実装する
            (model, Cmd.none)

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
                
                loadDataCmd route =
                    case route of
                        Detail uuid ->
                            let
                                cmd = 
                                    Http.getString
                                        (String.append model.config.detailUrl (String.append "/" uuid))
                                        
                                onResponse request =
                                    case request of
                                        Ok json -> LoadDataFromJson json
                                        Err err ->
                                            let
                                                _ = Debug.log "fail" err
                                            in
                                                NoOp
                            in
                                Http.send onResponse cmd
                        _ -> Cmd.none
            in
                ( { model | route = newRoute }, loadDataCmd newRoute )

        LoadDataFromJson json ->
            let
                _ = Debug.log "dfdfddd" [Decode.decodeString (Decode.field "profile" Decode.value) json]
                newModelState =
                    {
                        model |
                        uuid = (Decode.decodeString (Decode.at ["uuid"] Decode.string) json) |> (Result.withDefault ""),
                        isPrivate = (Decode.decodeString (Decode.at ["isPrivate"] Decode.bool) json) |> (Result.withDefault False),
                        tags = (Decode.decodeString (Decode.at ["tags"] (Decode.list Decode.string)) json) |> (Result.withDefault []),
                        profile = {
                            name = (Decode.decodeString (Decode.at ["profile", "name"] Decode.string) json) |> (Result.withDefault ""),
                            race = (Decode.decodeString (Decode.at ["profile", "race"] Decode.string) json) |> (Result.withDefault ""),
                            age = (Decode.decodeString (Decode.at ["profile", "age"] Decode.string) json) |> (Result.withDefault ""),
                            place =
                                let
                                    toPlace str =
                                        case str of
                                            "Purgatory" -> Purgatory
                                            "Garden" -> Garden 
                                            "Paradise" -> Paradise
                                            _ -> Purgatory

                                    decodePlace =
                                        Decode.string |> (Decode.map toPlace)
                                in
                                    (Decode.decodeString (Decode.at ["profile", "place"] decodePlace) json) |> (Result.withDefault Purgatory),                                    
                            height = (Decode.decodeString (Decode.at ["profile", "height"] Decode.string) json) |> (Result.withDefault ""),
                            weight = (Decode.decodeString (Decode.at ["profile", "weight"] Decode.string) json) |> (Result.withDefault ""),
                            implication = (Decode.decodeString (Decode.at ["profile", "implication"] Decode.string) json) |> (Result.withDefault ""),
                            hair = (Decode.decodeString (Decode.at ["profile", "hair"] Decode.string) json) |> (Result.withDefault ""),
                            eye = (Decode.decodeString (Decode.at ["profile", "eye"] Decode.string) json) |> (Result.withDefault ""),
                            skin = (Decode.decodeString (Decode.at ["profile", "skin"] Decode.string) json) |> (Result.withDefault ""),
                            memo = (Decode.decodeString (Decode.at ["profile", "memo"] Decode.string) json) |> (Result.withDefault ""),
                            memories = 
                                let
                                    toMemory str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                            description = (Decode.decodeString (Decode.field "description" Decode.string) str) |> (Result.withDefault "")
                                        }

                                    decodeMemory =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toMemory))
                                in
                                    (Decode.decodeString (Decode.at ["profile", "memories"] decodeMemory) json) |> (Result.withDefault []),
                            regrets =
                                let
                                    toRegret str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            target = (Decode.decodeString (Decode.field "target" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                            currentVal = (Decode.decodeString (Decode.field "currentVal" Decode.int) str) |> (Result.withDefault 0),
                                            maxVal = (Decode.decodeString (Decode.field "maxVal" Decode.int) str) |> (Result.withDefault 0),
                                            negative = (Decode.decodeString (Decode.field "negative" Decode.string) str) |> (Result.withDefault ""),
                                            description = (Decode.decodeString (Decode.field "description" Decode.string) str) |> (Result.withDefault "")
                                        }

                                    decodeRegret =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toRegret))
                                in
                                    (Decode.decodeString (Decode.at ["profile", "regrets"] decodeRegret) json) |> (Result.withDefault []),
                            karmas = 
                                let
                                    toKarma str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            achieved = (Decode.decodeString (Decode.field "achieved" Decode.bool) str) |> (Result.withDefault False),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                            description = (Decode.decodeString (Decode.field "description" Decode.string) str) |> (Result.withDefault "")
                                        }

                                    decodeKarma =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toKarma))
                                in
                                    (Decode.decodeString (Decode.at ["profile", "karmas"] decodeKarma) json) |> (Result.withDefault [])
                        },
                        classes = {
                            positions =
                                let
                                    toPosition str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault "")
                                        }

                                    decodePosition =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toPosition))

                                in
                                    (Decode.decodeString (Decode.at ["classes", "positions"] decodePosition) json) |> (Result.withDefault []),
                            subPositions =
                                let
                                    toSubPositions str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault "")
                                        }

                                    decodeSubPosition =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toSubPositions))

                                in
                                    (Decode.decodeString (Decode.at ["classes", "subPositions"] decodeSubPosition) json) |> (Result.withDefault []),
                            highTechs =
                                let
                                    toHighTech str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                            favor = (Decode.decodeString (Decode.field "favor" Decode.int) str) |> Result.toMaybe
                                        }

                                    decodeHighTech =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toHighTech))

                                in
                                    (Decode.decodeString (Decode.at ["classes", "highTechs"] decodeHighTech) json) |> (Result.withDefault []),
                            classes =
                                let
                                    toCategory str =
                                        case str of
                                            "MainClass" -> MainClass
                                            "SubClass" -> SubClass 
                                            "SecondClass" -> SecondClass
                                            "ThirdClass" -> ThirdClass
                                            "ThirdPointFiveClass" -> ThirdPointFiveClass
                                            "HighSociety" -> HighSociety
                                            "OtherClass" -> OtherClass
                                            _ -> MainClass

                                    toClasses str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            category = (Decode.decodeString (Decode.field "category" Decode.string) str) |> (Result.map toCategory) |> (Result.withDefault MainClass),
                                            from = (Decode.decodeString (Decode.field "from" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                            number = (Decode.decodeString (Decode.field "number" Decode.int) str) |> (Result.withDefault 0)
                                        }

                                    decodeClasses =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toClasses))

                                in
                                    (Decode.decodeString (Decode.at ["classes", "classes"] decodeClasses) json) |> (Result.withDefault []),
                            points =
                                let
                                    toPoints str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                            busou = (Decode.decodeString (Decode.field "busou" Decode.int) str) |> Result.toMaybe,
                                            heni = (Decode.decodeString (Decode.field "heni" Decode.int) str) |> Result.toMaybe,
                                            kaizou = (Decode.decodeString (Decode.field "kaizou" Decode.int) str) |> Result.toMaybe,
                                            favor = (Decode.decodeString (Decode.field "favor" Decode.int) str) |> Result.toMaybe
                                        }

                                    decodePoints =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toPoints))

                                in
                                    (Decode.decodeString (Decode.at ["classes", "points"] decodePoints) json) |> (Result.withDefault [])
                        },
                        favors =
                                let
                                    toFavor str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            personal = (Decode.decodeString (Decode.field "personal" Decode.int) str) |> Result.toMaybe,
                                            battle = (Decode.decodeString (Decode.field "battle" Decode.int) str) |> Result.toMaybe,
                                            memo = (Decode.decodeString (Decode.field "memo" Decode.string) str) |> (Result.withDefault "")
                                        }

                                    decodeFavor =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toFavor))

                                in
                                    (Decode.decodeString (Decode.at ["classes", "favors"] decodeFavor) json) |> (Result.withDefault []),
                        usedFavors =
                                let
                                    toUsedFavor str =
                                        {
                                            uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""),
                                            purpose = (Decode.decodeString (Decode.field "purpose" Decode.string) str) |> (Result.withDefault ""),
                                            memo = (Decode.decodeString (Decode.field "memo" Decode.string) str) |> (Result.withDefault ""),
                                            favor = (Decode.decodeString (Decode.field "favor" Decode.int) str) |> (Result.withDefault 0)
                                        }

                                    decodeUsedFavor =
                                        (Decode.list Decode.string) |> (Decode.map (List.map toUsedFavor))
                                in
                                    (Decode.decodeString (Decode.at ["classes", "usedFavors"] decodeUsedFavor) json) |> (Result.withDefault []),
                        tabs =
                            let
                                decodeManeuva =
                                    (Decode.list Decode.string) |> (Decode.map (List.map toManeuva))

                                toManeuvaType str =
                                    case str of
                                        "Skill" -> Skill
                                        "Part" -> Part
                                        "Item" -> Item
                                        "Effect" -> Effect
                                        "Archive" -> Archive
                                        "Tag" -> Tag
                                        _ -> Skill

                                toRegion str =
                                    case str of
                                        "NoRegion" -> NoRegion
                                        "Head" -> Head
                                        "Arm" -> Arm
                                        "Body" -> Body
                                        "Leg" -> Leg
                                        "OtherRegion" -> OtherRegion
                                        _ -> NoRegion

                                toTiming str =
                                    case str of
                                        "AutoAlways" -> AutoAlways
                                        "AutoNeedsDeclearation" -> AutoNeedsDeclearation
                                        "AutoOthers" -> AutoOthers
                                        "Action" -> Action
                                        "Judge" -> Judge
                                        "Damage" -> Damage
                                        "Rapid" -> Rapid
                                        "BeforeBattle" -> BeforeBattle
                                        "BattleStart" -> BattleStart
                                        "TurnStart" -> TurnStart
                                        "CountStart" -> CountStart
                                        _ -> AutoAlways

                                toPosition i =
                                    case i of
                                        Ok num -> Position num
                                        Err _ -> Position 0

                                toManeuva str =
                                    {
                                        uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) str) |> (Result.withDefault ""), 
                                        used = (Decode.decodeString (Decode.field "used" Decode.bool) str) |> (Result.withDefault False),
                                        lost = (Decode.decodeString (Decode.field "lost" Decode.bool) str) |> (Result.withDefault False),
                                        act = (Decode.decodeString (Decode.field "act" Decode.int) str) |> Result.toMaybe,
                                        maneuvaType = (Decode.decodeString (Decode.field "maneuvaType" Decode.string) str) |> (Result.map toManeuvaType) |> (Result.withDefault Skill),
                                        malice = (Decode.decodeString (Decode.field "malice" Decode.int) str) |> Result.toMaybe,
                                        favor = (Decode.decodeString (Decode.field "favor" Decode.int) str) |> Result.toMaybe,
                                        category = (Decode.decodeString (Decode.field "category" Decode.string) str) |> (Result.withDefault "0"),
                                        name = (Decode.decodeString (Decode.field "name" Decode.string) str) |> (Result.withDefault ""),
                                        timing = (Decode.decodeString (Decode.field "timing" Decode.string) str) |> (Result.map toTiming) |> (Result.withDefault AutoAlways),
                                        cost = (Decode.decodeString (Decode.field "cost" Decode.string) str) |> (Result.withDefault ""),
                                        range = (Decode.decodeString (Decode.field "range" Decode.string) str) |> (Result.withDefault ""),
                                        description = (Decode.decodeString (Decode.field "description" Decode.string) str) |> (Result.withDefault ""),
                                        from = (Decode.decodeString (Decode.field "from" Decode.string) str) |> (Result.withDefault ""),
                                        region = (Decode.decodeString (Decode.field "region" Decode.string) str) |> (Result.map toRegion) |> (Result.withDefault NoRegion),
                                        position = (Decode.decodeString (Decode.field "position" Decode.int) str) |> toPosition
                                    }

                                toManeuvaData str =
                                    ManeuvaTab 
                                        {
                                            dialogContent = Nothing,
                                            showAddManeuvaDialog = False,
                                            maneuvas = (Decode.decodeString (Decode.field "items" decodeManeuva) str) |> (Result.withDefault [])
                                        }
                                
                                toResource str =
                                    ResourceTab [
                                        {
                                            uuid = "",
                                            name = "",
                                            description = "",
                                            favor = Just 0,
                                            position = Position 0
                                        }
                                    ]

                                decodeTabType =
                                    (Decode.at ["tabs"] Decode.string)
                                        |> Decode.andThen (\tabType ->
                                            case tabType of
                                                "ManeuvaTab" -> decodeManeuvaData
                                                "ResourceTab" -> Decode.string |> (Decode.map toResource)
                                                _ -> decodeManeuvaData
                                        )

                                decodeManeuvaData =
                                    Decode.string |> (Decode.map toManeuvaData)

                                defaultTabType =
                                    ManeuvaTab 
                                        {
                                            dialogContent = Nothing,
                                            showAddManeuvaDialog = False,
                                            maneuvas = [
                                                {
                                                    uuid = "", 
                                                    used = False,
                                                    lost = False,
                                                    act = Nothing,
                                                    maneuvaType = Skill,
                                                    malice = Nothing,
                                                    favor = Nothing,
                                                    category = "0",
                                                    name = "",
                                                    timing = AutoAlways,
                                                    cost = "",
                                                    range = "",
                                                    description = "",
                                                    from = "",
                                                    region = NoRegion,
                                                    position = Position 0
                                                }
                                            ]
                                        }

                                toTab str =
                                    {
                                        uuid = (Decode.decodeString (Decode.at ["tabs", "uuid"] Decode.string) json) |> (Result.withDefault ""),
                                        title = (Decode.decodeString (Decode.at ["tabs", "title"] Decode.string) json) |> (Result.withDefault ""),
                                        tabType = (Decode.decodeString (Decode.at ["tabs", "tabType"] decodeTabType) json) |> (Result.withDefault defaultTabType),
                                        isEditing = False,
                                        mismatches = []
                                    }
                    
                                decodeTab =
                                    (Decode.list Decode.string) |> (Decode.map (List.map toTab))

                            in
                                (Decode.decodeString (Decode.at ["tabs"] decodeTab) json) |> (Result.withDefault [])
                    }
                dfdfd = Debug.log "" [newModelState]

            in
                (newModelState, Cmd.none)
