module Main exposing (..)

import Random.Pcg exposing (generate)
import Html exposing (Html, div, text, program)
import Messages exposing (Msg(..))
import Models exposing (Model, ActiveTab(..), AppendMode(..), Place(..))
import Update exposing (update)
import View exposing (view)
import Utils exposing (updateOnWay)

import Uuid.Barebones exposing (uuidStringGenerator)

init : ( Model, Cmd Msg )
init =
    let
        model = 
            {
                uuid = "",
                isPrivate = False,
                passhash = Just "dummy",
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

                    ],
                    points = []
                },
                favors = [],
                tabs = [
                    {
                        uuid = "",
                        title = "スキル",
                        isEditing = False,
                        items = []
                    },
                    {
                        uuid = "",
                        title = "パーツ",
                        isEditing = True,
                        items = []
                    }
                ],
                appendMode = AppendSkill
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
                ) model.classes.highTechs
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