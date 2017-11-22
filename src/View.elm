module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))
import Models exposing (Model, ActiveTab(..), AppendMode(..), Tab, Profile, Classes, Favor, Place(..), Regret)
import Utils exposing (..)
import Random.Pcg exposing (generate)
import Uuid.Barebones exposing (uuidStringGenerator)

stylesheet : String -> Html Msg
stylesheet path =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      path
            ]
        children = []
    in 
        node tag attrs children

view : Model -> Html Msg
view model =
    body [] [
        stylesheet "./css/style.css",
        stylesheet "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css",
        div [class "content"]
        [
            div [class "left"] [
                div [] [text "なんか"],
                div [] [text "Text出力とか"],
                div [] [text "保存とか"],
                div [] [text "なんか"],
                div [] [text "なんか"],
                div [] [text "なんか"]
            ],
            div [class "right"] [
                ul [class "tabcontrol"] (tabcontorls model),
                div [class "tabbody"] [
                    case model.activeTab of
                        ProfileTab -> profileTab model.profile
                        FavorsTab -> favorsTab model.favors
                        ClassesTab -> classesTab model.classes
                        OtherTab tab -> otherTab tab
                ]
            ]
        ]
    ]

profileTab : Profile -> Html Msg
profileTab profile =
    div [] [
        div [class "section-title"] [text "パーソナル"],
        table [] [
            tbody [] [
                tr [] [
                    th [colspan 2] [text "キャラクター名"],
                    td [colspan 4] [
                        input [size 55, type_ "text", value profile.name, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | name = s}}))] []
                    ]
                ],
                tr [] [
                    th [] [text "種族"],
                    td [] [
                        input [size 16, type_ "text", value profile.race, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | race = s}}))] []
                    ],
                    th [] [text "享年"],
                    td [] [
                        input [size 16, type_ "text", value profile.age, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | age = s}}))] []
                    ],
                    th [] [text "初期配置"],
                    td [] [
                        select [] [
                            option ([onInput (\s -> FormUpdated (\m -> {m | profile = {profile | place = Purgatory}}))] ++ (
                                case profile.place of
                                    Purgatory -> [selected True]
                                    _ -> []
                            )) [text "煉獄"],
                            option ([onInput (\s -> FormUpdated (\m -> {m | profile = {profile | place = Garden}}))] ++ (
                                case profile.place of
                                    Garden -> [selected True]
                                    _ -> []
                            )) [text "花園"],
                            option ([onInput (\s -> FormUpdated (\m -> {m | profile = {profile | place = Paradise}}))] ++ (
                                case profile.place of
                                    Paradise -> [selected True]
                                    _ -> []
                            )) [text "楽園"]
                        ]
                    ]
                ],
                tr [] [
                    th [] [text "身長"],
                    td [] [
                        input [size 16, type_ "text", value profile.height, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | height = s}}))] []
                    ],
                    th [] [text "体重"],
                    td [] [
                        input [size 16, type_ "text", value profile.weight, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | weight = s}}))] []
                    ],
                    th [] [text "暗示"],
                    td [] [
                        input [size 16, type_ "text", value profile.implication, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | implication = s}}))] []
                    ]
                ],
                tr [] [
                    th [] [text "髪の色"],
                    td [] [
                        input [size 16, type_ "text", value profile.hair, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | hair = s}}))] []
                    ],
                    th [] [text "瞳の色"],
                    td [] [
                        input [size 16, type_ "text", value profile.eye, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | eye = s}}))] []
                    ],
                    th [] [text "肌の色"],
                    td [] [
                        input [size 16, type_ "text", value profile.skin, onInput (\s -> FormUpdated (\m -> {m | profile = {profile | skin = s}}))] []
                    ]
                ]
            ]
        ],
        div [class "section-title"] [text "記憶のカケラ"],
        table [] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "175px")]] [text "名前"],
                        th [style [("width", "385px")]] [text "詳細"],
                        td [] []
                    ]
                ] ++ (List.map (\memory -> tr [] [
                    th [] [
                        input [
                            size 20,
                            type_ "text",
                            value memory.name,
                            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | memories = Utils.updateOnWay profile.memories memory (\mem -> {mem | name = s})}}))
                        ] []
                    ],
                    td [] [
                        input [
                            size 50,
                            type_ "text",
                            value memory.description,
                            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | memories = Utils.updateOnWay profile.memories memory (\mem -> {mem | description = s})}}))
                        ] []
                    ],
                    td [] [
                        span [
                            class "ion-close-round",
                            let
                                eq = \x -> x == memory
                            in
                                onClick (RemoveRow (\m -> {m | profile = {profile | memories = (Utils.takeNotWhile eq profile.memories) ++ (Utils.dropNotWhile eq profile.memories) }}))
                        ] []
                    ]
                ]) profile.memories)
            )
        ],
        div [] [
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | profile = {
                                        profile | memories = profile.memories ++ [
                                            {
                                                uuid = uuid,
                                                name = "",
                                                description = ""
                                            }
                                        ]
                                    }
                                }
                            )
                        ) uuidStringGenerator
                    )
                )
            ] [text "追加"]
        ],
        div [class "section-title"] [text "未練"],
        table [] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "175px")]] [text "対象"],
                        th [style [("width", "105px")]] [text "種類"],
                        th [style [("width", "50px")]] [text "現在値"],
                        th [style [("width", "50px")]] [text "最大値"],
                        th [style [("width", "175px")]] [text "発狂効果"],
                        th [style [("width", "315px")]] [text "備考"],
                        td [] []
                    ]
                ] ++ (List.map (\regret ->
                    tr [] [
                        td [] [
                            input [
                                size 20,
                                type_ "text",
                                value regret.target,
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | target = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                size 10,
                                type_ "text",
                                value regret.name,
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                Html.Attributes.min "0",
                                Html.Attributes.max (toString regret.maxVal),
                                value (toString regret.currentVal),
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | currentVal = Result.withDefault 0 (String.toInt s)})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                Html.Attributes.min "4",
                                value (toString regret.maxVal),
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | maxVal = Result.withDefault 0 (String.toInt s)})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                size 20,
                                type_ "text",
                                value regret.negative,
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | negative = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                size 40,
                                type_ "text",
                                value regret.description,
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | description = s})}}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == regret
                                in
                                    onClick (RemoveRow (\m -> {m | profile = {profile | regrets = (Utils.takeNotWhile eq profile.regrets) ++ (Utils.dropNotWhile eq profile.regrets) }}))
                            ] []
                        ]
                    ]
                ) profile.regrets)
            )
        ],
        div [] [
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | profile = {
                                        profile | regrets = profile.regrets ++ [
                                            {
                                                uuid = uuid,
                                                target = "",
                                                name = "",
                                                currentVal = 3,
                                                maxVal = 4,
                                                negative = "",
                                                description = ""
                                            }
                                        ]
                                    }
                                }
                            )
                        ) uuidStringGenerator
                    )
                )
            ] [text "追加"]
        ],
        div [class "section-title"] [text "カルマ"],
        table [] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "35px")]] [text "達成"],
                        th [style [("width", "175px")]] [text "条件"],
                        th [style [("width", "385px")]] [text "詳細"],
                        td [] []
                    ]
                ] ++ (List.map (\karma -> tr [] [
                    th [] [
                        input [
                            type_ "checkbox",
                            checked karma.achieved
                        ] []
                    ],
                    th [] [
                        input [
                            size 20,
                            type_ "text",
                            value karma.name,
                            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | name = s})}}))
                        ] []
                    ],
                    td [] [
                        input [
                            size 50,
                            type_ "text",
                            value karma.description,
                            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | description = s})}}))
                        ] []
                    ],
                    td [] [
                        span [
                            class "ion-close-round",
                            let
                                eq = \x -> x == karma
                            in
                                onClick (RemoveRow (\m -> {m | profile = {profile | karmas = (Utils.takeNotWhile eq profile.karmas) ++ (Utils.dropNotWhile eq profile.karmas) }}))
                        ] []
                    ]
                ]) profile.karmas)
            )
        ],
        div [] [
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | profile = {
                                        profile | karmas = profile.karmas ++ [
                                            {
                                                uuid = uuid,
                                                achieved = False,
                                                name = "",
                                                description = ""
                                            }
                                        ]
                                    }
                                }
                            )
                        ) uuidStringGenerator
                    )
            )
            ] [text "追加"]
        ]
    ]

favorsTab : List Favor -> Html Msg
favorsTab favors =
    div [] []

classesTab : Classes -> Html Msg
classesTab classes =
    div [] []

otherTab : Tab -> Html Msg
otherTab tab =
    div [] []

tabToLi : Model -> Tab -> Html Msg
tabToLi model currentTab =
    let
        classes = [class "tabctl", class "appendable", onClick (OtherTabClicked currentTab)] ++ case model.activeTab of
                OtherTab active -> if active == currentTab then [class "active"] else []
                _ -> []
    in
        li classes [
            span [class "ion-edit"] [],
            span [] [
                if currentTab.isEditing then 
                    input [type_ "text", value currentTab.title, size 10] [] else
                    text currentTab.title
            ],
            span [class "ion-close-round"] []
        ]

tabcontorls : Model -> List (Html Msg)
tabcontorls model = 
    let
        personalClass = [class "tabctl", onClick PersonalTabClicked] ++ case model.activeTab of
            ProfileTab -> [class "active"]
            _ -> []
        classesClass = [class "tabctl", onClick ClassesTabClicked] ++ case model.activeTab of
            ClassesTab -> [class "active"]
            _ -> []
        favorsClass = [class "tabctl", onClick FavorsTabClicked] ++ case model.activeTab of
            FavorsTab -> [class "active"]
            _ -> []
    in
        [   
            li personalClass [
                span [class "ion-locked"] [],
                span [class "tab-name"] [text "パーソナル"]
            ],
            li classesClass [
                span [class "ion-locked"] [],
                span [class "tab-name"] [text "クラス類"]
            ],
            li favorsClass [
                span [class "ion-locked"] [],
                span [class "tab-name"] [text "寵愛"]
            ]
        ] ++ 
        (List.map (tabToLi model) model.tabs) ++
        [
            li [class "tabctl", class "new-tab"] [
                span [class "ion-locked"] [],
                select [class "new-tab-type"] [
                    option [value "skill", selected (
                            case model.appendMode of
                                AppendSkill -> True
                                AppendPart -> False
                            )
                    ] [text "スキル"],
                    option [value "part", selected (
                            case model.appendMode of
                                AppendPart -> True
                                AppendSkill -> False
                            )
                    ] [text "パーツ"]
                ],
                span [class "add-tab", class "ion-plus-round"] []
            ]
        ]