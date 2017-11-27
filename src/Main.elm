module Main exposing (..)

import Random.Pcg exposing (generate, initialSeed, Seed, step)
import Html exposing (Html, div, text, programWithFlags)
import Messages exposing (Msg(..))
import Models exposing (..)
import Update exposing (update)
import View exposing (view)
import Utils exposing (updateOnWay)
import List.FlatMap exposing (..)
import Uuid.Barebones exposing (uuidStringGenerator)
import Window exposing (..)
import Task
import Html5.DragDrop as DragDrop

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model = 
            {
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
                        isEditing = False
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
        profile = model.profile
        classes = model.classes
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
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                                profile = a.profile
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        profile = {profile | memories = Utils.updateOnWay profile.memories x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.profile.memories
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        profile = {profile | regrets = Utils.updateOnWay profile.regrets x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.profile.regrets
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        profile = {profile | karmas = Utils.updateOnWay profile.karmas x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.profile.karmas
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        classes = {classes | positions = Utils.updateOnWay classes.positions x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.classes.positions
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        classes = {classes | subPositions = Utils.updateOnWay classes.subPositions x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.classes.subPositions
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        classes = {classes | highTechs = Utils.updateOnWay classes.highTechs x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.classes.highTechs
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        classes = {classes | classes = Utils.updateOnWay classes.classes x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.classes.classes
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        classes = {classes | points = Utils.updateOnWay classes.points x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.classes.points
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        classes = {classes | points = Utils.updateOnWay classes.points x (\y -> {y | uuid = uuid})}
                                }
                        ) m m.classes.points
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        favors = Utils.updateOnWay model.favors x (\y -> {y | uuid = uuid})
                                }
                        ) m m.favors
                )
                |> (\m  ->
                    List.foldr 
                        (\x a ->
                            let
                                (uuid, newSeed) = step uuidStringGenerator a.seed
                            in
                                {
                                    a |
                                        seed = newSeed,
                                        usedFavors = Utils.updateOnWay model.usedFavors x (\y -> {y | uuid = uuid})
                                }
                        ) m m.usedFavors
                )
                |> (\m ->
                    List.foldr
                        (\x a ->
                            let
                                maneuvas = 
                                    case x.tabType of
                                        ManeuvaTab tabData -> tabData.maneuvas
                                        _ -> []

                                (uuid, newSeed) = step uuidStringGenerator a.seed

                                newModel = {
                                    a |
                                        seed = newSeed,
                                        tabs = Utils.updateOnWay a.tabs x (\y -> {y | uuid = uuid})
                                }
                                _ = Debug.log "fdfdf" [newModel]
                            in
                                List.foldr (\y b ->
                                    let
                                        (uuid2, newSeed2) = step uuidStringGenerator b.seed

                                        newTabState =
                                            {
                                                x |
                                                    tabType =
                                                        case x.tabType of
                                                            ManeuvaTab tabData ->
                                                                ManeuvaTab {
                                                                    tabData | 
                                                                        maneuvas = Utils.updateOnWay tabData.maneuvas y (\c -> {
                                                                            c |
                                                                                uuid = uuid2
                                                                        })
                                                                }
                                                            _ -> x.tabType
                                            }
                                    in
                                        {
                                            b |
                                                seed = newSeed2,
                                                tabs = Utils.updateOnWay b.tabs x (\z -> newTabState)
                                        }
                                ) newModel maneuvas
                        ) m m.tabs
                )
                ,
            Cmd.none
        )
    
                    -- -- 頑張ってUUIDで初期IDを振っている
            -- Cmd.batch (
            --     List.FlatMap.flatMap (\tab -> 
            --         (
            --             List.map (\item -> 
            --                 generate (\uuid -> FormUpdated (\m ->
            --                     let
            --                         tabs = case List.head (List.reverse m.tabs) of
            --                             Just tab -> Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb ->
            --                                 {tb | tabType = 
            --                                     case tb.tabType of
            --                                         ManeuvaTab tabData ->
            --                                             let
            --                                                 newManeuvas =
            --                                                     Utils.updateOnWayUseEq tabData.maneuvas (\x -> x.uuid == "") item (\ma -> {ma | uuid = uuid})
            --                                             in
            --                                                 ManeuvaTab {tabData | maneuvas = newManeuvas}
            --                                         _ -> tb.tabType
            --                                 }
            --                             )
            --                             Nothing -> []
            --                     in
            --                         {m | 
            --                             tabs = tabs
            --                         }
            --                     )
            --                 ) uuidStringGenerator
            --             ) (case tab.tabType of
            --                 ManeuvaTab tabData -> tabData.maneuvas
            --                 _ -> []
            --             )
            --         ) ++
            --         (
            --             List.map (\resource -> 
            --                 generate (\uuid -> FormUpdated (\m ->
            --                     let
            --                         tabs = case List.head (List.reverse m.tabs) of
            --                             Just tab -> Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb ->
            --                                 {tb | tabType = 
            --                                     case tb.tabType of
            --                                         ResourceTab resources ->
            --                                             ResourceTab (
            --                                                 let 
            --                                                     newManeuvas =
            --                                                         Utils.updateOnWayUseEq resources (\x -> x.uuid == "") resource (\ma -> {ma | uuid = uuid})
            --                                                 in
            --                                                     newManeuvas
            --                                             )
            --                                         _ -> tb.tabType
            --                                 }
            --                             )
            --                             Nothing -> []
            --                     in
            --                         {m | 
            --                             tabs = tabs
            --                         }
            --                     )
            --                 ) uuidStringGenerator
            --             ) (case tab.tabType of
            --                 ResourceTab resources -> resources
            --                 _ -> []
            --             )
            --         ) ++ 
            --         [
            --             generate (\x -> 
            --                 FormUpdated (\m -> {m | tabs = Utils.updateOnWay model.tabs tab (\tb -> {tb | uuid = x})})
            --             ) uuidStringGenerator
            --         ]
            --     ) model.tabs ++
            --     List.map (\favor -> 
            --         generate (\x -> 
            --             FormUpdated (\m -> {m | favors = Utils.updateOnWay model.favors favor (\fv -> {fv | uuid = x})})
            --         ) uuidStringGenerator
            --     ) model.favors ++
            --     List.map (\used -> 
            --         generate (\x -> 
            --             FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay model.usedFavors used (\us -> {us | uuid = x})})
            --         ) uuidStringGenerator
            --     ) model.usedFavors ++
            --     [(Task.perform SetWindowSize Window.size)]
            -- )
        -- )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- MAIN
main : Program Flags Model Msg
main =
    programWithFlags
        {
            init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
        }