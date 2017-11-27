module Models exposing (..)

import Window exposing (Size)
import Html5.DragDrop as DragDrop
import Random.Pcg

type alias Flags =
    {
        randomSeed: Int
    }

type alias Model =
    {
        uuid: String,
        isPrivate: Bool,
        password: Maybe String,
        tagform: String,
        tags: List String,
        activeTab: ActiveTab,
        profile: Profile,
        favors: List Favor,
        usedFavors: List UsedFavor,
        classes: Classes,
        tabs: List Tab,
        appendMode: AppendMode,
        saveMode: SaveMode,
        showDeleteTabialog: Maybe Tab,
        windowSize: Size,
        dragDrop : DragDrop.Model String Position,
        seed: Random.Pcg.Seed
    }

type Position
    = Position Int

type alias Resource =
    {
        uuid: String,
        name: String,
        description: String,
        favor: Maybe Int,
        position: Position -- 行内の位置
    }

type AppendMode =
    AppendManeuva | AppendResource

type SaveMode =
    UpdateSheet | CloneSheet

type ActiveTab =
    ProfileTab | FavorsTab | ClassesTab | OtherTab Tab

type alias Favor =
    {
        uuid: String,
        personal: Maybe Int,
        battle: Maybe Int,
        memo: String
    }

type alias UsedFavor =
    {
        uuid: String,
        purpose: String,
        favor: Int,
        memo: String
    }

type Place =
    Purgatory | Garden | Paradise

type alias Memory =
    {
        uuid: String,
        name: String,
        description: String
    }

type alias Regret =
    {
        uuid: String,
        target: String,
        name: String,
        currentVal: Int,
        maxVal: Int,
        negative: String,
        description: String
    }

type alias Karma =
    {
        uuid: String,
        achieved: Bool,
        name: String,
        description: String
    }

type alias Profile =
    {
        name: String,
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

type alias PositionDetail =
    {
        uuid: String,
        name: String
    }

type alias SubPositionDetail =
    {
        uuid: String,
        name: String
    }

type alias HighTechDetail =
    {
        uuid: String,
        name: String,
        favor: Maybe Int
    }

type ClassCategory =
    MainClass | SubClass | SecondClass | ThirdClass | ThirdPointFiveClass | HighSociety | OtherClass

type alias ClassDetail =
    {
        uuid: String,
        category: ClassCategory,
        from: String,
        name: String,
        number: Int
    }

type alias Point =
    {
        uuid: String,
        name: String,
        busou: Maybe Int,
        heni: Maybe Int,
        kaizou: Maybe Int,
        favor: Maybe Int
    }

type alias Classes =
    {
        positions: List PositionDetail,
        subPositions: List SubPositionDetail,
        highTechs: List HighTechDetail,
        classes: List ClassDetail,
        points: List Point
    }

type TabType =
    ManeuvaTab {
        maneuvas: List Maneuva,
        showAddManeuvaDialog: Bool,
        dialogContent: Maybe String
    } |
    ResourceTab (List Resource)

type alias Tab =
    {
        uuid: String,
        tabType: TabType,
        title: String,
        isEditing: Bool
    }

type ManeuvaType =
    Skill | Part | Item | Effect | Archive | Tag

type alias Maneuva =
    {
        uuid: String,
        used: Bool, -- 使用済み
        lost: Bool, -- 破損済み
        act: Maybe Int, -- 行動値
        malice: Maybe Int, -- 悪意
        favor: Maybe Int,  -- 寵愛
        maneuvaType: ManeuvaType, -- マニューバタイプ
        category: String, -- カテゴリ
        name: String, -- パーツ名
        timing: Timing, -- タイミング
        cost: String, -- コスト
        range: String, -- 射程
        description: String, -- 説明
        from: String, -- 取得元
        region: Region, -- 部位
        position: Position -- 行内の位置
    }

type Region =
    NoRegion | Head | Arm | Body | Leg | OtherRegion

type Timing =
    AutoAlways | AutoNeedsDeclearation | AutoOthers | Action | Judge | Damage | Rapid | BeforeBattle | BattleStart | TurnStart | CountStart