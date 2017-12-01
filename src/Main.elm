module Main exposing (..)

import Random.Pcg exposing (generate, initialSeed, Seed, step)
import Messages exposing (Msg(..))
import Models exposing (..)
import Update exposing (update)
import View exposing (view)
import Utils exposing (updateOnWay)
import Uuid.Barebones exposing (uuidStringGenerator)
import Html5.DragDrop as DragDrop
import Navigation exposing (Location, programWithFlags)
import Routing

init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        model = 
            {
                route = Routing.parseLocation location,
                config = flags.config,
                uuid = "",
                isPrivate = False,
                password = Nothing,
                tagform = "",
                tags = [],
                activeTab = ProfileTab,
                seed = initialSeed flags.randomSeed,
                profile = {
                    name = "",
                    race = "",
                    age = "",
                    place = Purgatory,
                    height = "",
                    weight = "",
                    implication = "",
                    hair = "",
                    eye = "",
                    skin = "",
                    memo = "",
                    memories = [
                        {
                            uuid = "",
                            name = "",
                            description = ""
                        },
                        {
                            uuid = "",
                            name = "",
                            description = ""
                        }
                    ],
                    regrets = [
                        {
                            uuid = "",
                            target = "たからもの",
                            name = "依存",
                            currentVal = 3,
                            maxVal = 4,
                            negative = "最大行動値減少(-2)",
                            description = "パーツとして所持。破壊で狂気点+1"
                        }
                    ],
                    karmas = [
                        {
                            uuid = "",
                            achieved = False,
                            name = "記憶のカケラを獲得する",
                            description = ""
                        }
                    ]
                },
                classes = {
                    positions = [
                        {
                            uuid = "",
                            name = ""
                        }
                    ],
                    subPositions = [
                        {
                            uuid = "",
                            name = ""
                        }
                    ],
                    highTechs = [
                        {
                            uuid = "",
                            name = "",
                            favor = Nothing
                        }
                    ],
                    classes = [
                        {
                            uuid = "",
                            category = MainClass,
                            from = "初期",
                            name = "",
                            number = 1
                        },
                        {
                            uuid = "",
                            category = SubClass,
                            from = "初期",
                            name = "",
                            number = 1
                        }
                    ],
                    points = [
                        {
                            uuid = "",
                            name = "",
                            busou = Nothing,
                            heni = Nothing,
                            kaizou = Nothing,
                            favor = Nothing
                        },
                        {
                            uuid = "",
                            name = "",
                            busou = Nothing,
                            heni = Nothing,
                            kaizou = Nothing,
                            favor = Nothing
                        },
                        {
                            uuid = "",
                            name = "ボーナス",
                            busou = Nothing,
                            heni = Nothing,
                            kaizou = Nothing,
                            favor = Nothing
                        }
                    ]
                },
                favors = [
                    {
                        uuid = "",
                        battle = Nothing,
                        personal = Nothing,
                        memo = ""
                    },
                    {
                        uuid = "",
                        battle = Nothing,
                        personal = Nothing,
                        memo = ""
                    },
                    {
                        uuid = "",
                        battle = Nothing,
                        personal = Nothing,
                        memo = ""
                    }
                ],
                usedFavors = [
                    {
                        uuid = "",
                        purpose = "",
                        favor = 0,
                        memo = ""
                    },
                    {
                        uuid = "",
                        purpose = "",
                        favor = 0,
                        memo = ""
                    }
                ],
                tabs = [
                    {
                        uuid = "",
                        title = "マニューバ",
                        tabType = ManeuvaTab 
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
                            },
                        isEditing = False,
                        mismatches = []
                    }
                ],
                appendMode = AppendManeuva,
                saveMode = UpdateSheet,
                showDeleteTabialog = Nothing,
                windowSize = {
                    width = 0,
                    height = 0
                },
                dragDrop = DragDrop.init
            }
    in
        (
            model
                |> (\m -> 
                        let
                            (uuid, newSeed) = step uuidStringGenerator model.seed
                        in
                            {
                                m |
                                    uuid = uuid,
                                    seed = newSeed
                            }
                    )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.profile.memories
                        profile = m.profile
                    in
                        {
                            m |
                                seed = lastSeed,
                                profile = {profile | memories = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.profile.regrets
                        profile = m.profile
                    in
                        {
                            m |
                                seed = lastSeed,
                                profile = {profile | regrets = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.profile.karmas
                        profile = m.profile
                    in
                        {
                            m |
                                seed = lastSeed,
                                profile = {profile | karmas = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.classes.positions
                        classes = m.classes
                    in
                        {
                            m |
                                seed = lastSeed,
                                classes = {classes | positions = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.classes.subPositions
                        classes = m.classes
                    in
                        {
                            m |
                                seed = lastSeed,
                                classes = {classes | subPositions = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.classes.highTechs
                        classes = m.classes
                    in
                        {
                            m |
                                seed = lastSeed,
                                classes = {classes | highTechs = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.classes.classes
                        classes = m.classes
                    in
                        {
                            m |
                                seed = lastSeed,
                                classes = {classes | classes = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.classes.points
                        classes = m.classes
                    in
                        {
                            m |
                                seed = lastSeed,
                                classes = {classes | points = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped }
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.favors
                    in
                        {
                            m |
                                seed = lastSeed,
                                favors = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.usedFavors
                    in
                        {
                            m |
                                seed = lastSeed,
                                usedFavors = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped
                        }                    
                )
                |> (\m  ->
                    let
                        (lastSeed, zipped) = Utils.zipWithUuid m.seed m.tabs
                    in
                        {
                            m |
                                seed = lastSeed,
                                tabs = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped
                        }                    
                )
                |> (\m  ->
                    let
                        newModelState =
                            List.foldr
                                (\tab newM ->
                                    let
                                        (lastSeed, newTabType) =
                                            case tab.tabType of
                                                ManeuvaTab tabData -> 
                                                    let
                                                        (lastSeed, zipped) = Utils.zipWithUuid newM.seed tabData.maneuvas
                                                    in
                                                        (
                                                            lastSeed,
                                                            ManeuvaTab {
                                                                tabData |
                                                                    maneuvas = List.map (\(uuid, elem) -> {elem | uuid = uuid}) zipped
                                                            }
                                                        )
                                                _ -> (newM.seed, tab.tabType)
                                    in
                                        { newM |
                                            seed = lastSeed,
                                            tabs = Utils.updateOnWay newM.tabs tab (\x -> {x | tabType = newTabType})
                                        }
                                )
                                m
                                m.tabs
                    in
                        newModelState
                ),
            Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    getDomSize UpdateSize

-- MAIN
main : Program Flags Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
