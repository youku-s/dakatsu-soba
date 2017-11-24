module Main exposing (..)

import Random.Pcg exposing (generate)
import Html exposing (Html, div, text, program)
import Messages exposing (Msg(..))
import Models exposing (..)
import Update exposing (update)
import View exposing (view)
import Utils exposing (updateOnWay)
import List.FlatMap exposing (..)
import Uuid.Barebones exposing (uuidStringGenerator)

init : ( Model, Cmd Msg )
init =
    let
        model = 
            {
                uuid = "",
                isPrivate = False,
                password = Nothing,
                tagform = "",
                tags = [],
                activeTab = ProfileTab,
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
                        tabType = ManeuvaTab [
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
                                region = Head
                            }
                        ],
                        isEditing = False
                    }
                ],
                appendMode = AppendManeuva,
                saveMode = UpdateSheet
            }
        profile = model.profile
        classes = model.classes
    in
        (
            model,
            -- 頑張ってUUIDで初期IDを振っている
            Cmd.batch (
                [
                    generate (\x -> FormUpdated (\m -> {m | uuid = x})) uuidStringGenerator
                ] ++
                List.map (\memory -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | profile = {profile | memories = Utils.updateOnWay profile.memories memory (\mem -> {mem | uuid = x})}})
                    ) uuidStringGenerator
                ) model.profile.memories ++
                List.map (\regret -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | uuid = x})}})
                    ) uuidStringGenerator
                ) model.profile.regrets ++
                List.map (\karma -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | uuid = x})}})
                    ) uuidStringGenerator
                ) model.profile.karmas ++
                List.map (\position -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | classes = {classes | positions = Utils.updateOnWay classes.positions position (\pos -> {pos | uuid = x})}})
                    ) uuidStringGenerator
                ) model.classes.positions ++
                List.map (\subPosition -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | classes = {classes | subPositions = Utils.updateOnWay classes.subPositions subPosition (\sup -> {sup | uuid = x})}})
                    ) uuidStringGenerator
                ) model.classes.subPositions ++
                List.map (\highTech -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | classes = {classes | highTechs = Utils.updateOnWay classes.highTechs highTech (\ht -> {ht | uuid = x})}})
                    ) uuidStringGenerator
                ) model.classes.highTechs ++
                List.map (\clazz -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | uuid = x})}})
                    ) uuidStringGenerator
                ) model.classes.classes ++
                List.map (\point -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\pt -> {pt | uuid = x})}})
                    ) uuidStringGenerator
                ) model.classes.points ++
                List.FlatMap.flatMap (\tab -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | tabs = Utils.updateOnWay model.tabs tab (\tb -> {tb | uuid = x})})
                    ) uuidStringGenerator ::
                    (List.map (\item -> 
                        generate (\x -> 
                            FormUpdated (\m -> {m | tabs = Utils.updateOnWay model.tabs tab (\tb -> {tb | tabType = 
                                case tb.tabType of
                                    ManeuvaTab items -> ManeuvaTab (Utils.updateOnWay items item (\it -> {it | uuid = x}))
                                    _ -> tb.tabType
                            }
                            )})
                        ) uuidStringGenerator
                    ) (case tab.tabType of
                        ManeuvaTab items -> items
                        _ -> []
                    )
                    )
                ) model.tabs ++
                List.map (\favor -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | favors = Utils.updateOnWay model.favors favor (\fv -> {fv | uuid = x})})
                    ) uuidStringGenerator
                ) model.favors ++
                List.map (\used -> 
                    generate (\x -> 
                        FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay model.usedFavors used (\us -> {us | uuid = x})})
                    ) uuidStringGenerator
                ) model.usedFavors
            )
        )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- MAIN
main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }