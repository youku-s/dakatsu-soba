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
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, resolve)
import Navigation

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

                newUrl =
                    if String.contains "detail" model.location.href then model.location.href else String.concat [model.location.href, "#detail/", model.uuid]

                onResponse response =
                    case response of
                        Ok _ -> OnLoad (Just newUrl) (Ok ["保存に成功しました。", (String.concat ["あなたのキャラクターシートのURLは [", newUrl, "] です。"])])
                        Err _ -> OnLoad Nothing (Err ["保存に失敗しました。時間をおいて、もう一度保存を試してみてください。"])
            in
                (
                    model,
                    Http.send onResponse request
                )
        
        Output ->
            let
                -- FIXME 手抜き。あとで直す
                strToValue s =
                    Encode.string s

                intToValue i =
                    Encode.int i

                placeToString p =
                    case p of
                        Purgatory -> "煉獄"
                        Garden -> "花園"
                        Paradise -> "楽園"

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
                        MainClass -> "メインクラス"
                        SubClass -> "サブクラス"
                        SecondClass -> "2次クラス"
                        ThirdClass -> "3次クラス"
                        ThirdPointFiveClass -> "3.5次クラス"
                        HighSociety -> "HS"
                        OtherClass -> "その他"

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
                        Skill -> "スキル"
                        Part -> "パーツ"
                        Item -> "アイテム"
                        Effect -> "エフェクト"
                        Archive -> "アーカイブ"
                        Tag -> "タグ"

                regionToStr re =
                    case re of
                        NoRegion -> "-"
                        Head -> "頭"
                        Arm -> "腕"
                        Body -> "胴"
                        Leg -> "脚"
                        OtherRegion -> "その他"

                timingToStr ti =
                    case ti of
                        AutoAlways -> "オート(常時)"
                        AutoNeedsDeclearation -> "オート(宣言)"
                        AutoOthers -> "オート"
                        Action -> "アクション"
                        Judge -> "ジャッジ"
                        Damage -> "ダメージ"
                        Rapid -> "ラピッド"
                        BeforeBattle -> "戦闘開始前"
                        BattleStart -> "戦闘開始時"
                        TurnStart -> "ターン開始"
                        CountStart -> "カウント開始"

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
                    post model.config.outputUrl (model |> modelToValue |> Http.jsonBody)

                onResponse response =
                    NoOp
            in
                (
                    { model | outputQuery = (model |> modelToValue |> (Encode.encode 0)) },
                    Cmd.none
                )

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
                toPlace str =
                    case str of
                        "Purgatory" -> Purgatory
                        "Garden" -> Garden 
                        "Paradise" -> Paradise
                        _ -> Purgatory

                decodePlace =
                    Decode.string |> (Decode.map toPlace)

                decodeMemory =
                    decode Memory
                        |> required "uuid" Decode.string
                        |> optional "name" Decode.string ""
                        |> optional "description" Decode.string ""

                decodeRegret =
                    decode Regret
                        |> required "uuid" Decode.string
                        |> optional "target" Decode.string ""
                        |> optional "name" Decode.string ""
                        |> optional "currentVal" Decode.int 0
                        |> optional "maxVal" Decode.int 0
                        |> optional "negative" Decode.string ""
                        |> optional "description" Decode.string ""

                decodeKarma =
                    decode Karma
                        |> required "uuid" Decode.string
                        |> optional "achieved" Decode.bool False
                        |> optional "name" Decode.string ""
                        |> optional "description" Decode.string ""

                decodeProfile =
                    decode Profile
                        |> optional "name" Decode.string ""
                        |> optional "race" Decode.string ""
                        |> optional "age" Decode.string ""
                        |> optional "place" decodePlace Purgatory
                        |> optional "height" Decode.string ""
                        |> optional "weight" Decode.string ""
                        |> optional "implication" Decode.string ""
                        |> optional "hair" Decode.string ""
                        |> optional "eye" Decode.string ""
                        |> optional "skin" Decode.string ""
                        |> optional "memo" Decode.string ""
                        |> optional "memories" (Decode.list decodeMemory) []
                        |> optional "regrets" (Decode.list decodeRegret) []
                        |> optional "karmas" (Decode.list decodeKarma) []

                decodePositionDetail =
                    decode PositionDetail
                        |> required "uuid" Decode.string
                        |> optional "name" Decode.string ""

                decodeSubPosition =
                    decode SubPositionDetail
                        |> required "uuid" Decode.string
                        |> optional "name" Decode.string ""

                decodeHighTech =
                    decode HighTechDetail
                        |> required "uuid" Decode.string
                        |> optional "name" Decode.string ""
                        |> optional "favor" (Decode.maybe Decode.int) Nothing

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

                decodeClassDetail =
                    decode ClassDetail
                        |> required "uuid" Decode.string
                        |> optional "category" (Decode.string |> (Decode.map toCategory)) MainClass
                        |> optional "name" Decode.string ""
                        |> optional "from" Decode.string ""
                        |> optional "number" Decode.int 0

                decodePoint =
                    decode Point
                        |> required "uuid" Decode.string
                        |> optional "name" Decode.string ""
                        |> optional "busou" (Decode.maybe Decode.int) Nothing
                        |> optional "heni" (Decode.maybe Decode.int) Nothing
                        |> optional "kaizou" (Decode.maybe Decode.int) Nothing
                        |> optional "favor" (Decode.maybe Decode.int) Nothing

                decodeClasses =
                    decode Classes
                        |> optional "positions" (Decode.list decodePositionDetail) []
                        |> optional "subPositions" (Decode.list decodeSubPosition) []
                        |> optional "highTechs" (Decode.list decodeHighTech) []
                        |> optional "classes" (Decode.list decodeClassDetail) []
                        |> optional "points" (Decode.list decodePoint) []

                decodeFavor =
                    decode Favor
                        |> required "uuid" Decode.string
                        |> optional "personal" (Decode.maybe Decode.int) Nothing
                        |> optional "battle" (Decode.maybe Decode.int) Nothing
                        |> optional "memo" Decode.string ""

                decodeUsedFavor =
                    decode UsedFavor
                        |> required "uuid" Decode.string
                        |> optional "purpose" Decode.string ""
                        |> optional "favor" Decode.int 0
                        |> optional "memo" Decode.string ""

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

                decodeManeuva =
                    decode Maneuva
                        |> required "uuid" Decode.string
                        |> optional "used" Decode.bool False
                        |> optional "lost" Decode.bool False
                        |> optional "act" (Decode.maybe Decode.int) Nothing
                        |> optional "malice" (Decode.maybe Decode.int) Nothing
                        |> optional "favor" (Decode.maybe Decode.int) Nothing
                        |> optional "maneuvaType" (Decode.string |> (Decode.map toManeuvaType)) Skill
                        |> optional "category" Decode.string "0"
                        |> optional "name" Decode.string ""
                        |> optional "timing" (Decode.string |> (Decode.map toTiming)) AutoAlways
                        |> optional "cost" Decode.string ""
                        |> optional "range" Decode.string ""
                        |> optional "description" Decode.string ""
                        |> optional "from" Decode.string ""
                        |> optional "region" (Decode.string |> (Decode.map toRegion)) NoRegion
                        |> required "position" (Decode.int |> (Decode.map Position))

                decodeManeuvaDetail =
                    decode ManeuvaTabDetail
                        |> required "maneuvas" (Decode.list decodeManeuva)
                        |> hardcoded False
                        |> hardcoded Nothing
                        
                decodeResource =
                    decode Resource
                        |> required "uuid" Decode.string
                        |> optional "name" Decode.string ""
                        |> optional "description" Decode.string ""
                        |> optional "favor" (Decode.maybe Decode.int) Nothing
                        |> required "position" (Decode.int |> (Decode.map Position))

                decodeTabType =
                    Decode.andThen (\tabType ->
                            case tabType of
                                "ManeuvaTab" -> decodeManeuvaDetail |> (Decode.map ManeuvaTab)
                                "ResourceTab" -> (Decode.list decodeResource) |> (Decode.map ResourceTab)
                                _ -> decodeManeuvaDetail |> (Decode.map ManeuvaTab)
                        )
                
                toDecoder uuid tabType maneuvas resources title isEditing mismatches =
                    case tabType of 
                        "ManeuvaTab" -> Decode.succeed({
                            uuid = uuid,
                            tabType = ManeuvaTab {
                                maneuvas = maneuvas,
                                showAddManeuvaDialog = False,
                                dialogContent = Nothing
                            },
                            title = title,
                            isEditing = isEditing,
                            mismatches = mismatches
                        })
                        "ResourceTab" -> Decode.succeed({
                            uuid = uuid,
                            tabType = ResourceTab resources,
                            title = title,
                            isEditing = isEditing,
                            mismatches = mismatches
                        })
                        _ -> Decode.fail "JSONからのキャラクターシート復元に失敗しました"
                
                decodeTab : Decode.Decoder Tab
                decodeTab =
                    decode toDecoder
                        |> required "uuid" Decode.string
                        |> required "tabType" Decode.string 
                        |> optional "items" (Decode.list decodeManeuva) []
                        |> optional "items" (Decode.list decodeResource) []
                        |> optional "title" Decode.string ""
                        |> hardcoded False -- isEditing
                        |> hardcoded [] -- mismatches
                        |> resolve

                newModelState =
                    {
                        model |
                        uuid = (Decode.decodeString (Decode.field "uuid" Decode.string) json) |> (Result.withDefault ""),
                        isPrivate = (Decode.decodeString (Decode.field "isPrivate" Decode.bool) json) |> (Result.withDefault False),
                        tags = (Decode.decodeString (Decode.field "tags" (Decode.list Decode.string)) json) |> (Result.withDefault []),
                        profile = (Decode.decodeString (Decode.field "profile" decodeProfile) json) |> (Result.withDefault model.profile),
                        classes = (Decode.decodeString (Decode.field "classes" decodeClasses) json) |> (Result.withDefault model.classes),
                        favors = (Decode.decodeString (Decode.field "favors" (Decode.list decodeFavor)) json) |> (Result.withDefault []),
                        usedFavors = (Decode.decodeString (Decode.field "usedFavors" (Decode.list decodeUsedFavor)) json) |> (Result.withDefault []),
                        tabs = (Decode.decodeString (Decode.field "tabs" (Decode.list decodeTab)) json) |> (Result.withDefault [])
                    }
            in
                (newModelState, Cmd.none)

        ShowResult message ->
            ({model | result = Just message}, Cmd.none)
        
        HideResult ->
            ({model | result = Nothing}, Cmd.none)

        OnLoad newUrl message ->
            let
                cmd =
                    case newUrl of
                        Just url -> Navigation.newUrl url
                        Nothing -> Cmd.none
            in
                (
                    {model | result = Just message },
                    cmd
                )