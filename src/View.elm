module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))
import Models exposing (..)
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
                                class "number",
                                type_ "number",
                                Html.Attributes.min "0",
                                Html.Attributes.max (toString regret.maxVal),
                                value (toString regret.currentVal),
                                onInput (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | currentVal = Result.withDefault 0 (String.toInt s)})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                class "number",
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
                            checked karma.achieved,
                            onCheck (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | achieved = s})}}))
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
        ],
        div [class "section-title"] [text "メモ"],
        textarea [
            style [("width", "1180px")],
            rows (Basics.max (List.length (String.lines profile.memo) + 1) 5),
            onInput (\s -> FormUpdated (\m -> {m | profile = {profile | memo = s }}))
        ] [text profile.memo]
    ]

favorsTab : List Favor -> Html Msg
favorsTab favors =
    div [] []

classesTab : Classes -> Html Msg
classesTab classes =
    div [] [
        div [class "section-title"] [text "ポジション"],
        div [class "position"] [
            table [] [
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
                                size 20,
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
            table [] [
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
                                size 20,
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
            table [] [
                tbody [] ([
                    tr [] [
                            th [style [("width", "175px")]] [text "ハイテック"],
                            th [style [("width", "85px")]] [text "寵愛"],
                            td [style [("width", "15px")]] []
                        ]
                    ] ++ (List.map (\highTech -> tr [] [
                        td [] [
                            input [
                                size 20,
                                type_ "text",
                                value highTech.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | highTechs = Utils.updateOnWay classes.highTechs highTech (\x -> {highTech | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                class "favor",
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
        table [] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "105px")]] [text "種別"],
                        th [style [("width", "105px")]] [text "取得元"],
                        th [style [("width", "245px")]] [text "クラス名"],
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
                                size 10,
                                type_ "text",
                                value clazz.from,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | from = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                size 30,
                                type_ "text",
                                value clazz.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                class "number",
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
        table [] [
            tbody [] (
                [
                    tr [] [
                        th [style [("width", "105px")]] [text "クラス名"],
                        th [style [("width", "85px")]] [text "武装"],
                        th [style [("width", "85px")]] [text "変異"],
                        th [style [("width", "85px")]] [text "改造"],
                        th [style [("width", "85px")]] [text "寵愛"],
                        td [style [("width", "15px")]] []
                    ]
                ] ++ (List.map (\point -> tr [] [
                        td [] [
                            input [
                                size 10,
                                type_ "text",
                                value point.name,
                                onInput (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                class "point",
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
                                class "point",
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
                                class "point",
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
                                class "favor",
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
                                size 10,
                                type_ "text",
                                value "総計",
                                readonly True
                            ] []
                        ],
                        td [] [
                            input [
                                class "point",
                                type_ "text",
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
                                class "point",
                                type_ "text",
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
                                class "point",
                                type_ "text",
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
                                class "favor",
                                type_ "text",
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
    div [] []

tabToLi : Model -> Tab -> Html Msg
tabToLi model currentTab =
    let
        classes = [class "tabctl", class "appendable", onClick (OtherTabClicked currentTab)] ++ case model.activeTab of
                OtherTab active -> if active == currentTab then [class "active"] else []
                _ -> []
    in
        li classes [
            span [
                class "ion-edit",
                onClick (ToggleEdit currentTab)
            ] [],
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