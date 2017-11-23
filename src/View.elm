module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, onCheck, onWithOptions, targetValue)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)
import Random.Pcg exposing (generate)
import Json.Decode
import Uuid.Barebones exposing (uuidStringGenerator)
import List.FlatMap exposing (..)

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
                div [] [
                    label [] [text "パスワード"],
                    input [
                        type_ "password",
                        size 10,
                        onInput (\s -> FormUpdated (\m -> {m | password = Just s}))
                    ] [],
                    button [] [text "保存"]
                ],
                div [] [
                    input [type_ "radio", name "save-option"] [],
                    span [] [text "通常の保存"]
                ],
                div [] [
                    input [type_ "radio", name "save-option"] [],
                    span [] [text "シートをコピーする"]
                ],
                div [] [
                    input [type_ "checkbox"] [],
                    span [] [text "プライベートモード"]
                ],
                div [] [
                    button [] [text "Text出力"]
                ],
                div [] [text "なんか"],
                div [] [text "なんか"],
                div [] [text "なんか"]
            ],
            div [class "right"] [
                ul [class "tabcontrol"] (tabcontorls model),
                div [class "tabbody"] [
                    case model.activeTab of
                        ProfileTab -> profileTab model.profile
                        FavorsTab -> favorsTab model
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
                        select [
                            let 
                                toPlace = \str -> case str of
                                    "煉獄" -> Purgatory
                                    "花園" -> Garden
                                    _ -> Paradise
                            in
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | place = toPlace(s)}}))
                        ] [
                            option [selected (
                                case profile.place of
                                    Purgatory -> True
                                    _ -> False
                            ), value "煉獄"] [text "煉獄"],
                            option [selected (
                                case profile.place of
                                    Garden -> True
                                    _ -> False
                            ), value "花園"] [text "花園"],
                            option [selected (
                                case profile.place of
                                    Paradise -> True
                                    _ -> False
                            ), value "楽園"] [text "楽園"]
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
                            type_ "text",
                            value memory.name,
                            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | memories = Utils.updateOnWay profile.memories memory (\mem -> {mem | name = s})}}))
                        ] []
                    ],
                    td [] [
                        input [
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
                                type_ "text",
                                value regret.target,
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | target = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
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
                                type_ "text",
                                value regret.negative,
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | negative = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
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
                            checked karma.achieved,
                            onCheck (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | achieved = s})}}))
                        ] []
                    ],
                    th [] [
                        input [
                            type_ "text",
                            value karma.name,
                            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | name = s})}}))
                        ] []
                    ],
                    td [] [
                        input [
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
        ],
        div [class "section-title"] [text "メモ"],
        textarea [
            style [("width", "90%")],
            rows (Basics.max (List.length (String.lines profile.memo) + 1) 5),
            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | memo = s }}))
        ] [text profile.memo]
    ]

favorsTab : Model -> Html Msg
favorsTab model =
    let
        favors = model.favors
        usedFavors = model.usedFavors
        tabs = model.tabs
        points = model.classes.points
        highTechs = model.classes.highTechs
    in
        div [] [
            div [class "section-title"] [text "獲得寵愛点"],
            table [style[("width", "465px")]] [
                tbody [] ([
                    tr [] [
                            th [style [("width", "50px")]] [text "戦闘"],
                            th [style [("width", "50px")]] [text "個人"],
                            th [style [("width", "50px")]] [text "総計"],
                            th [style [("width", "300px")]] [text "備考"],
                            td [style [("width", "15px")]] []
                        ]
                    ] ++ (List.map (\favor -> tr [] [
                        td [] [
                            input [
                                type_ "number",
                                Html.Attributes.min "0",
                                value (
                                    case favor.battle of
                                        Just favor -> toString favor
                                        Nothing -> ""
                                ),
                                onInput (\s -> FormUpdated (\m -> {m | favors = Utils.updateOnWay favors favor (\x -> {favor | battle = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                Html.Attributes.min "0",
                                value (
                                    case favor.personal of
                                        Just favor -> toString favor
                                        Nothing -> ""
                                ),
                                onInput (\s -> FormUpdated (\m -> {m | favors = Utils.updateOnWay favors favor (\x -> {favor | personal = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value (
                                    toString ((Maybe.withDefault 0 favor.battle) + (Maybe.withDefault 0 favor.personal))
                                ),
                                readonly True
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value favor.memo,
                                onInput (\s -> FormUpdated (\m -> {m | favors = Utils.updateOnWay favors favor (\x -> {favor | memo = s})}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x.uuid == favor.uuid
                                in
                                    onClick (RemoveRow (\m -> {m | favors = (Utils.takeNotWhile eq favors) ++ (Utils.dropNotWhile eq favors)}))
                            ] []
                        ]
                    ]) favors)
                )
            ],
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | favors = (
                                        favors ++ [
                                            {
                                                uuid = uuid,
                                                memo = "",
                                                battle = Nothing,
                                                personal = Nothing
                                            }
                                        ]
                                    )
                                }
                            )
                        ) uuidStringGenerator
                    )
            )
            ] [text "追加"],
            div [class "section-title"] [text "使用済み寵愛点"],
            table [style[("width", "540px")]] [
                tbody [] ([
                        tr [] [
                            th [style [("width", "175px")]] [text "目的"],
                            th [style [("width", "50px")]] [text "消費"],
                            th [style [("width", "300px")]] [text "備考"],
                            td [style [("width", "15px")]] []
                        ],
                        tr [] [
                            td [] [
                                input [
                                    type_ "text",
                                    value "スキル",
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    value (
                                        let
                                            isSkillTab = \x -> case x.tabType of
                                                SkillTab -> True
                                                _ -> False
                                            skillTabs = List.filter isSkillTab tabs
                                            unWrap = \x -> case x of
                                                Skill data -> data.favor
                                                Part data -> Nothing
                                            totalAsTab = \x -> List.map (\y -> Maybe.withDefault 0 (unWrap y)) x.items
                                            total = List.sum (flatMap totalAsTab skillTabs)
                                        in
                                            toString total
                                    ),
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value "",
                                    readonly True
                                ] []
                            ],
                            td [] []
                        ],
                        tr [] [
                            td [] [
                                input [
                                    type_ "text",
                                    value "パーツ",
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    value (
                                        let
                                            isPartTab = \x -> case x.tabType of
                                                PartTab -> True
                                                _ -> False
                                            partTab = List.filter isPartTab tabs
                                            unWrap = \x -> case x of
                                                Skill data -> Nothing
                                                Part data -> data.favor
                                            totalAsTab = \x -> List.map (\y -> Maybe.withDefault 0 (unWrap y)) x.items
                                            total = List.sum (flatMap totalAsTab partTab)
                                        in
                                            toString total
                                    ),
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value "",
                                    readonly True
                                ] []
                            ],
                            td [] []
                        ],
                        tr [] [
                            td [] [
                                input [
                                    type_ "text",
                                    value "強化値",
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    value (List.sum (List.map (\x -> Maybe.withDefault 0 x.favor) points) |> toString),
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value "",
                                    readonly True
                                ] []
                            ],
                            td [] []
                        ],
                        tr [] [
                            td [] [
                                input [
                                    type_ "text",
                                    value "ポジション類",
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    value (List.sum (List.map (\x -> Maybe.withDefault 0 x.favor) highTechs) |> toString),
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value "",
                                    readonly True
                                ] []
                            ],
                            td [] []
                        ]
                    ] ++ (List.map (\used -> tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value used.purpose,
                                onInput (\s -> FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay usedFavors used (\x -> {used | purpose = s})}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                Html.Attributes.min "0",
                                value (toString used.favor),
                                onInput (\s -> FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay usedFavors used (\x -> {used | favor = 
                                    case String.toInt s of
                                        Ok num -> num
                                        Err _ -> 0
                                })}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value used.memo,
                                onInput (\s -> FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay usedFavors used (\x -> {used | memo = s})}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x.uuid == used.uuid
                                in
                                    onClick (RemoveRow (\m -> {m | usedFavors = (Utils.takeNotWhile eq usedFavors) ++ (Utils.dropNotWhile eq usedFavors)}))
                            ] []
                        ]
                    ]) usedFavors)
                )
            ],
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | usedFavors = (
                                        usedFavors ++ [
                                            {
                                                uuid = uuid,
                                                purpose = "",
                                                memo = "",
                                                favor = 0
                                            }
                                        ]
                                    )
                                }
                            )
                        ) uuidStringGenerator
                    )
                )
            ] [text "追加"]
        ]

classesTab : Classes -> Html Msg
classesTab classes =
    div [] [
        div [class "section-title"] [text "ポジション"],
        div [class "position"] [
            table [style[("width", "190px")]] [
                tbody [] (
                    [
                        tr [] [
                            th [style [("width", "175px")]] [text "ポジション"],
                            td [style [("width", "15px")]] []
                        ]
                    ] ++
                    (List.map (\position -> tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value position.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | positions = Utils.updateOnWay classes.positions position (\x -> {position | name = s})}}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == position
                                in
                                    onClick (RemoveRow (\m -> {m | classes = {classes | positions = (Utils.takeNotWhile eq classes.positions) ++ (Utils.dropNotWhile eq classes.positions) }}))
                            ] []
                        ]
                    ]) classes.positions)
                )
            ],
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | classes = {
                                        classes | positions = (
                                            classes.positions ++ [
                                                {
                                                    uuid = uuid,
                                                    name = ""
                                                }
                                            ]
                                        )
                                    }
                                }
                            )
                        ) uuidStringGenerator
                    )
            )
            ] [text "追加"]
        ],
        div [class "position"] [
            table [style[("width", "190px")]] [
                tbody [] (
                    [
                        tr [] [
                            th [style [("width", "175px")]] [text "サブポジション"],
                            td [style [("width", "15px")]] []
                        ]
                    ] ++
                    (List.map (\subPosition -> tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value subPosition.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | subPositions = Utils.updateOnWay classes.subPositions subPosition (\x -> {subPosition | name = s})}}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == subPosition
                                in
                                    onClick (RemoveRow (\m -> {m | classes = {classes | subPositions = (Utils.takeNotWhile eq classes.subPositions) ++ (Utils.dropNotWhile eq classes.subPositions) }}))
                            ] []
                        ]
                    ]) classes.subPositions)
                )
            ],
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | classes = {
                                        classes | subPositions = (
                                            classes.subPositions ++ [
                                                {
                                                    uuid = uuid,
                                                    name = ""
                                                }
                                            ]
                                        )
                                    }
                                }
                            )
                        ) uuidStringGenerator
                    )
            )
            ] [text "追加"]
        ],
        div [class "position"] [
            table [style[("width", "240px")]] [
                tbody [] ([
                    tr [] [
                            th [style [("width", "175px")]] [text "ハイテック"],
                            th [style [("width", "50px")]] [text "寵愛"],
                            td [style [("width", "15px")]] []
                        ]
                    ] ++ (List.map (\highTech -> tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value highTech.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | highTechs = Utils.updateOnWay classes.highTechs highTech (\x -> {highTech | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value (
                                    case highTech.favor of
                                        Just favor -> toString favor
                                        Nothing -> ""
                                ),
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | highTechs = Utils.updateOnWay classes.highTechs highTech (\x -> {highTech | favor = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == highTech
                                in
                                    onClick (RemoveRow (\m -> {m | classes = {classes | highTechs = (Utils.takeNotWhile eq classes.highTechs) ++ (Utils.dropNotWhile eq classes.highTechs)}}))
                            ] []
                        ]
                    ]) classes.highTechs)
                )
            ],
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | classes = {
                                        classes | highTechs = (
                                            classes.highTechs ++ [
                                                {
                                                    uuid = uuid,
                                                    name = "",
                                                    favor = Nothing
                                                }
                                            ]
                                        )
                                    }
                                }
                            )
                        ) uuidStringGenerator
                    )
            )
            ] [text "追加"]
        ],
        div [class "section-title"] [text "クラス"],
        table [style[("width", "470px")]] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "110px")]] [text "種別"],
                        th [style [("width", "110px")]] [text "取得元"],
                        th [style [("width", "200px")]] [text "クラス名"],
                        th [style [("width", "50px")]] [text "個数"],
                        td [] []
                    ]
                ] ++ (List.map (\clazz ->
                    tr [] [
                        td [] [
                            select [
                                let 
                                    toCategory = \str -> case str of
                                        "メインクラス" -> MainClass
                                        "サブクラス" -> SubClass
                                        "2次クラス" -> SecondClass
                                        "3次クラス" -> ThirdClass
                                        "3.5次クラス" -> ThirdPointFiveClass
                                        "HS" -> HighSociety
                                        _ -> OtherClass
                                in
                                    onInput (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | category = toCategory(s)})}}))
                            ] [
                                option [selected (
                                    case clazz.category of
                                        MainClass -> True
                                        _ -> False
                                ), value "メインクラス"] [text "メインクラス"],
                                option [selected (
                                    case clazz.category of
                                        SubClass -> True
                                        _ -> False
                                ), value "サブクラス"] [text "サブクラス"],
                                option [selected (
                                    case clazz.category of
                                        SecondClass -> True
                                        _ -> False
                                ), value "2次クラス"] [text "2次クラス"],
                                option [selected (
                                    case clazz.category of
                                        ThirdClass -> True
                                        _ -> False
                                ), value "3次クラス"] [text "3次クラス"],
                                option [selected (
                                    case clazz.category of
                                        ThirdPointFiveClass -> True
                                        _ -> False
                                ), value "3.5次クラス"] [text "3.5次クラス"],
                                option [selected (
                                    case clazz.category of
                                        HighSociety -> True
                                        _ -> False
                                ), value "HS"] [text "HS"],
                                option [selected (
                                    case clazz.category of
                                        OtherClass -> True
                                        _ -> False
                                ), value "その他"] [text "その他"]
                            ]
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value clazz.from,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | from = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value clazz.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                Html.Attributes.min "0",
                                value (toString clazz.number),
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | number = Result.withDefault 0 (String.toInt s)})}}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == clazz
                                in
                                    onClick (RemoveRow (\m -> {m | classes = {classes | classes = (Utils.takeNotWhile eq classes.classes) ++ (Utils.dropNotWhile eq classes.classes) }}))
                            ] []
                        ]
                    ]
                ) classes.classes)
            )
        ],
        div [] [
            button [
                type_ "button",
                onClick (AddRow 
                    (\m -> 
                        generate (\uuid -> 
                            FormUpdated (\m -> {m | classes = {
                                        classes | classes = classes.classes ++ [
                                            {
                                                uuid = uuid,
                                                category = MainClass,
                                                name = "",
                                                from = "",
                                                number = 0
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
        div [class "section-title"] [text "強化値"],
        table [style[("width", "365px")]] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "150px")]] [text "クラス名"],
                        th [style [("width", "50px")]] [text "武装"],
                        th [style [("width", "50px")]] [text "変異"],
                        th [style [("width", "50px")]] [text "改造"],
                        th [style [("width", "50px")]] [text "寵愛"],
                        td [style [("width", "15px")]] []
                    ]
                ] ++ (List.map (\point -> tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value point.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value (
                                    case point.busou of
                                        Just busou -> toString busou
                                        Nothing -> ""
                                ),
                                Html.Attributes.min "0",
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | busou = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value (
                                    case point.heni of
                                        Just heni -> toString heni
                                        Nothing -> ""
                                ),
                                Html.Attributes.min "0",
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | heni = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value (
                                    case point.kaizou of
                                        Just kaizou -> toString kaizou
                                        Nothing -> ""
                                ),
                                Html.Attributes.min "0",
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | kaizou = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value (
                                    case point.favor of
                                        Just favor -> toString favor
                                        Nothing -> ""
                                ),
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | favor = 
                                    case String.toInt s of
                                        Ok num -> Just num
                                        Err _ -> Nothing
                                })}}))
                            ] []
                        ],
                        td [] [
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == point
                                in
                                    onClick (RemoveRow (\m -> {m | classes = {classes | points = (Utils.takeNotWhile eq classes.points) ++ (Utils.dropNotWhile eq classes.points)}}))
                            ] []
                        ]
                    ]
                ) classes.points) ++ [
                    tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value "総計",
                                readonly True
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value ((List.sum (List.map (\x -> 
                                    case x.busou of
                                        Just num -> num
                                        Nothing -> 0
                                ) classes.points)) |> toString),
                                readonly True
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value ((List.sum (List.map (\x -> 
                                    case x.heni of
                                        Just num -> num
                                        Nothing -> 0
                                ) classes.points)) |> toString),
                                readonly True
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value ((List.sum (List.map (\x -> 
                                    case x.kaizou of
                                        Just num -> num
                                        Nothing -> 0
                                ) classes.points)) |> toString),
                                readonly True
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "number",
                                value ((List.sum (List.map (\x -> 
                                    case x.favor of
                                        Just num -> num
                                        Nothing -> 0
                                ) classes.points)) |> toString),
                                readonly True
                            ] []
                        ],
                        td [] []
                    ]
                ]
            )
        ],
        button [
            type_ "button",
            onClick (AddRow 
                (\m -> 
                    generate (\uuid -> 
                        FormUpdated (\m -> {m | classes = {
                                    classes | points = (
                                        classes.points ++ [
                                            {
                                                uuid = uuid,
                                                name = "",
                                                busou = Nothing,
                                                heni = Nothing,
                                                kaizou = Nothing,
                                                favor = Nothing
                                            }
                                        ]
                                    )
                                }
                            }
                        )
                    ) uuidStringGenerator
                )
        )
        ] [text "追加"]
    ]

otherTab : Tab -> Html Msg
otherTab tab =
    div [] [
        div [class "section-title"] [text tab.title],
        let
            tabbody =
                case tab.tabType of
                    SkillTab -> skillTab tab
                    PartTab -> div [] []
        in
            tabbody
    ]

skillTab : Tab -> Html Msg
skillTab tab =
    let
        filterF = \x ->
            case x of
                Skill _ -> True
                _ -> False
        items = List.filter filterF tab.items
    in
        div [] [
            table [style [("width", "1315px")]] [
                tbody [] ([
                    tr [] [
                        th [style [("width", "50px")]] [text "No."],
                        th [style [("width", "50px")]] [text "損傷"],
                        th [style [("width", "50px")]] [text "使用"],
                        th [style [("width", "50px")]] [text "悪意"],
                        th [style [("width", "50px")]] [text "行動値"],
                        th [style [("width", "100px")]] [text "種別"],
                        th [style [("width", "100px")]] [text "マニューバ"],
                        th [style [("width", "150px")]] [text "タイミング"],
                        th [style [("width", "60px")]] [text "コスト"],
                        th [style [("width", "50px")]] [text "射程"],
                        th [style [("width", "300px")]] [text "効果"],
                        th [style [("width", "250px")], colspan 2] [text "取得先"],
                        th [style [("width", "50px")]] [text "寵愛"],
                        td [style [("width", "15px")]] []
                    ]
                ] ++ (List.map (\(index, item) -> tr [] [
                    th [] [text (toString index)],
                    th [] [
                        let
                            boxChecked =
                                case item of
                                    Skill data -> data.lost
                                    _ -> False
                        in
                            input [
                                type_ "checkbox",
                                checked boxChecked,
                                onCheck (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                        case x of
                                            Skill data -> Skill {data | lost = s }
                                            _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    th [] [
                        let
                            boxChecked =
                                case item of
                                    Skill data -> data.used
                                    _ -> False
                        in
                            input [
                                type_ "checkbox",
                                checked boxChecked,
                                onCheck (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                        case x of
                                            Skill data -> Skill {data | used = s }
                                            _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> case data.malice of
                                        Just malice -> toString malice
                                        Nothing -> ""
                                    _ -> ""
                        in
                            input [
                                type_ "number",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | malice = (case String.toInt s of
                                            Ok num -> Just num
                                            _ -> Nothing
                                        ) }
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> case data.act of
                                        Just act -> toString act
                                        Nothing -> ""
                                    _ -> ""
                        in
                            input [
                                type_ "number",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | act = (case String.toInt s of
                                            Ok num -> Just num
                                            _ -> Nothing
                                        ) }
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> data.category
                                    _ -> ""
                        in
                            select [
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | category = s}
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "通常技"],
                                option [value "1", selected (val == "1")] [text "必殺技"],
                                option [value "2", selected (val == "2")] [text "行動値増加"],
                                option [value "3", selected (val == "3")] [text "補助"],
                                option [value "4", selected (val == "4")] [text "妨害"],
                                option [value "5", selected (val == "5")] [text "防御/生贄"],
                                option [value "6", selected (val == "6")] [text "移動"]
                            ]
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> data.name
                                    _ -> ""
                        in
                            input [
                                type_ "text",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | name = s}
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> case data.timing of
                                        AutoAlways -> "0"
                                        AutoNeedsDeclearation -> "1"
                                        AutoOthers -> "2"
                                        Action -> "3"
                                        Judge -> "4"
                                        Damage -> "5"
                                        Rapid -> "6"
                                    _ -> ""
                        in
                            select [
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | timing = case s of
                                            "0" -> AutoAlways
                                            "1" -> AutoNeedsDeclearation
                                            "2" -> AutoOthers
                                            "3" -> Action
                                            "4" -> Judge
                                            "5" -> Damage
                                            "6" -> Rapid
                                            _ -> AutoAlways
                                        }
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "オート(常時)"],
                                option [value "1", selected (val == "1")] [text "オート(宣言)"],
                                option [value "2", selected (val == "2")] [text "オート(その他)"],
                                option [value "3", selected (val == "3")] [text "アクション"],
                                option [value "4", selected (val == "4")] [text "ジャッジ"],
                                option [value "5", selected (val == "5")] [text "ダメージ"],
                                option [value "6", selected (val == "6")] [text "ラピッド"]
                            ]
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> data.cost
                                    _ -> ""
                        in
                            input [
                                type_ "text",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | cost = s}
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> data.range
                                    _ -> ""
                        in
                            input [
                                type_ "text",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | range = s}
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> data.description
                                    _ -> ""
                        in
                            input [
                                type_ "text",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | description = s}
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> case data.skillFrom of
                                        None -> "0"
                                        PositionSkill -> "1"
                                        SubPositionSkill -> "2"
                                        HighPositionSkill -> "3"
                                        HighTechSkill -> "4"
                                        MainClassSkill -> "5"
                                        SubClassSkill -> "6"
                                        SecondClassSkill -> "7"
                                        CUGSkill -> "8"
                                        ThirdClassSkill -> "9"
                                        ThirdPointFiveClassSkill -> "10"
                                        HighSocietySkill -> "11"
                                        SenarioSkill -> "12"
                                        OtherSkill -> "13"
                                    _ -> ""
                        in
                            select [
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | skillFrom = case s of
                                            "0" -> None
                                            "1" -> PositionSkill
                                            "2" -> SubPositionSkill
                                            "3" -> HighPositionSkill
                                            "4" -> HighTechSkill
                                            "5" -> MainClassSkill
                                            "6" -> SubClassSkill
                                            "7" -> SecondClassSkill
                                            "8" -> CUGSkill
                                            "9" -> ThirdClassSkill
                                            "10" -> ThirdPointFiveClassSkill
                                            "11" -> HighSocietySkill
                                            "12" -> SenarioSkill
                                            "13" -> OtherSkill
                                            _ -> None
                                        }
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text ""],
                                option [value "1", selected (val == "1")] [text "ポジション"],
                                option [value "2", selected (val == "2")] [text "サブポジション"],
                                option [value "3", selected (val == "3")] [text "ハイポジション"],
                                option [value "4", selected (val == "4")] [text "ハイテック"],
                                option [value "5", selected (val == "5")] [text "メインクラス"],
                                option [value "6", selected (val == "6")] [text "サブクラス"],
                                option [value "7", selected (val == "7")] [text "2次クラス"],
                                option [value "8", selected (val == "8")] [text "CUG"],
                                option [value "9", selected (val == "9")] [text "3次クラス"],
                                option [value "10", selected (val == "10")] [text "3.5次クラス"],
                                option [value "11", selected (val == "11")] [text "HS"],
                                option [value "12", selected (val == "12")] [text "シナリオ"],
                                option [value "13", selected (val == "13")] [text "その他"]
                            ]
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> data.from
                                    _ -> ""
                        in
                            input [
                                type_ "text",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | from = s}
                                        _ -> x

                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item of
                                    Skill data -> case data.favor of
                                        Just favor -> toString favor
                                        Nothing -> ""
                                    _ -> ""
                        in
                            input [
                                type_ "number",
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | items = Utils.updateOnWay tab.items item (\x -> 
                                    case x of
                                        Skill data -> Skill {data | favor = (case String.toInt s of
                                            Ok num -> Just num
                                            _ -> Nothing
                                        ) }
                                        _ -> x
                                    )}
                                in
                                    {m |
                                        tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        span [
                            class "ion-close-round",
                            let
                                eq = \x -> x == item
                                newTabState = {tab | items = (Utils.takeNotWhile eq tab.items) ++ (Utils.dropNotWhile eq tab.items)}
                            in
                                onClick (RemoveRow (\m -> {m |
                                    activeTab = OtherTab newTabState,
                                    tabs = Utils.updateOnWay m.tabs tab (\x -> newTabState)
                                }))
                        ] []
                    ]
                ]) (Utils.zipWithIndex items)))
            ],
            button [
                type_ "button",
                onClick (AddRow 
                    (\model -> 
                        generate (\uuid -> FormUpdated (\m -> 
                            let
                                newTabState = {tab | items = tab.items ++ [
                                    Skill {
                                        uuid = uuid, 
                                        used = False,
                                        lost = False,
                                        act = Nothing,
                                        malice = Nothing,
                                        favor = Nothing,
                                        category = "0",
                                        name = "",
                                        timing = AutoAlways,
                                        cost = "",
                                        range = "",
                                        description = "",
                                        skillFrom = None,
                                        from = ""
                                    }
                                ]}
                            in
                            {m | 
                                activeTab = OtherTab newTabState,
                                tabs = Utils.updateOnWay m.tabs tab (\tb -> newTabState)
                            }
                        )
                        ) uuidStringGenerator
                    )
                )
            ] [text "追加"]
        ]

onClickNoBubble : Msg -> Attribute Msg
onClickNoBubble msg =
    let
        options = {
            stopPropagation = True,
            preventDefault = True
        }
    in
        onWithOptions "click" options (Json.Decode.succeed msg)

tabToLi : Model -> Tab -> Html Msg
tabToLi model currentTab =
    let
        classes = [class "tabctl", class "appendable", onClick (OtherTabClicked currentTab)] ++ case model.activeTab of
                OtherTab active -> if active.uuid == currentTab.uuid then [class "active"] else []
                _ -> []
    in
        li classes [
            span [
                class "ion-edit",
                onClickNoBubble (ToggleEdit currentTab)
            ] [],
            span [] [
                if currentTab.isEditing then 
                    input [type_ "text", value currentTab.title, size 10] [] else
                    text currentTab.title
            ],
            span [
                class "ion-close-round",
                onClickNoBubble (RemoveTab currentTab)
            ] []
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