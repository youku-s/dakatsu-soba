module Main exposing (..)

import Html exposing (Html, div, text, program)
import Messages exposing (Msg)
import Models exposing (Model, ActiveTab(..), AppendMode(..), Place(..))
import Update exposing (update)
import View exposing (view)

init : ( Model, Cmd Msg )
init =
    (
        {
            activeTab = ProfileTab,
            profile = {
                name = "",
                tags = [],
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
                memories = [],
                regrets = [
                    {
                        target = "たからもの",
                        name = "依存",
                        current = 3,
                        max = 4,
                        negative = "最大行動値減少(-2)",
                        description = "パーツとして所持。破壊で狂気点+1"
                    }
                ],
                karmas = [
                    {
                        achieved = False,
                        name = "記憶のカケラを獲得する",
                        description = ""
                    }
                ]
            },
            classes = {
                positions = [],
                subPositions = [],
                highTechs = [],
                classes = [],
                points = []
            },
            favors = [],
            tabs = [],
            appendMode = AppendSkill
        },
        Cmd.none
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