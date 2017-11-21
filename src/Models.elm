module Models exposing (..)

type alias Model =
    {
        activeTab: ActiveTab,
        profile: Profile,
        favors: List Favor,
        classes: Classes,
        tabs: List Tab,
        appendMode: AppendMode
    }

type AppendMode =
    AppendSkill | AppendPart

type ActiveTab =
    ProfileTab | FavorsTab | ClassesTab | OtherTab Tab

type alias Favor =
    {
        personal: Maybe Int,
        battle: Maybe Int,
        memo: String
    }

type Place =
    Purgatory | Garden | Paradice

type alias Memory =
    {
        name: String,
        description: String
    }

type alias Regret =
    {
        target: String,
        name: String,
        current: Int,
        max: Int,
        negative: String,
        description: String
    }

type alias Karma =
    {
        achieved: Bool,
        name: String,
        description: String
    }

type alias Profile =
    {
        name: String,
        tags: List String,
        race: String,
        age: String,
        place: Place,
        height: String,
        weight: String,
        implication: String,
        hair: String,
        eye: String,
        skin: String,
        memo: String,
        memories: List Memory,
        regrets: List Regret,
        karmas: List Karma
    }

type alias HighTechDetail =
    {
        name: String,
        favor: Maybe Int
    }

type ClassCategory =
    MainClass | SubClass | SecondClass | ThirdClass | ThirdPointFiveClass | HighSociety

type alias ClassDetail =
    {
        category: ClassCategory,
        from: String,
        name: String,
        number: Int
    }

type alias Point =
    {
        name: String,
        busou: Maybe Int,
        heni: Maybe Int,
        kaizou: Maybe Int,
        favor: Maybe Int
    }

type alias Classes =
    {
        positions: List String,
        subPositions: List String,
        highTechs: List HighTechDetail,
        classes: List ClassDetail,
        points: List Point
    }

type alias Tab =
    {
        title: String,
        isEditing: Bool,
        items: List Maneuva
    }

type Maneuva =
    Skill {
        used: Bool, -- 使用済み
        lost: Bool, -- 破損済み
        act: Maybe Int, -- 行動値
        malice: Maybe Int, -- 悪意
        favor: Maybe Int,  -- 寵愛
        category: String, -- 種別
        name: String, -- スキル名
        timing: Timing, -- タイミング
        cost: String, -- コスト
        range: String, -- 射程
        description: String, -- 説明
        from: SkillFrom -- 取得元
    } |
    Part {
        used: Bool, -- 使用済み
        lost: Bool, -- 破損済み
        act: Maybe Int, -- 行動値
        malice: Maybe Int, -- 悪意
        favor: Maybe Int,  -- 寵愛
        category: String, -- 種別
        name: String, -- パーツ名
        timing: Timing, -- タイミング
        cost: String, -- コスト
        range: String, -- 射程
        description: String, -- 説明
        from: String -- 取得元
    }

type Timing =
    AutoAlways | AutoNeedsDeclearation | AutoOthers | Action | Judge | Damage | Rapid

type SkillFrom =
    None | PositionSkill String | SubPositionSkill String | HighPositionSkill String |
    HighTechSkill String | MainClassSkill String | SubClassSkill String | SecondClassSkill String |
    CUGSkill String | ThirdClassSkill String | ThirdPointFiveClassSkill String | HighSocietySkill String |
    SenarioSkill | OtherSkill String