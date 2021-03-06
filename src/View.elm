module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, onCheck, onSubmit, onWithOptions, targetValue, on, keyCode, onWithOptions, defaultOptions, onMouseDown)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)
import Random.Pcg exposing (generate)
import Json.Decode
import Uuid.Barebones exposing (uuidStringGenerator)
import List.FlatMap exposing (..)
import Html5.DragDrop as DragDrop

view : Model -> Html Msg
view model =
    div []
        [ page model ]

page : Model -> Html Msg
page model =
    case model.route of
        NewDetail ->
            detailPage model

        Detail id ->
            detailPage model

        NotFoundRoute ->
            notFoundView

notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]

detailPage : Model -> Html Msg
detailPage model =
    body [] ([
        div [class "content"]
        [
            div [class "left"] [
                div [class "section-title"] [text "パスワード"],
                div [] [
                    input [
                        type_ "password",
                        onChange (\s -> FormUpdated (\m -> {m | password = if String.isEmpty s then Nothing else Just s}))
                    ] []
                ],
                div [] [
                    button [
                        onClick Save
                    ] [text "保存"]
                ],
                div [] [
                    input [
                        type_ "radio",
                        name "save-option",
                        checked (case model.saveMode of
                            UpdateSheet -> True
                            _ -> False
                        ),
                        onInput (\s -> FormUpdated (\m -> {m | saveMode = UpdateSheet}))
                    ] [],
                    span [] [text "通常の保存"]
                ],
                div [] [
                    input [
                        type_ "radio",
                        name "save-option",
                        checked (case model.saveMode of
                            CloneSheet -> True
                            _ -> False
                        ),
                        onInput (\s -> FormUpdated (\m -> {m | saveMode = CloneSheet}))
                    ] [],
                    span [] [text "シートをコピーする"]
                ],
                div [] [
                    input [
                        type_ "radio",
                        name "save-option",
                        checked (case model.saveMode of
                            DeleteSheet -> True
                            _ -> False
                        ),
                        onInput (\s -> FormUpdated (\m -> {m | saveMode = DeleteSheet}))
                    ] [],
                    span [] [text "シートを削除する"]
                ],
                div [] [
                    input [
                        type_ "checkbox",
                        checked model.isPrivate,
                        onCheck (\b -> FormUpdated (\m -> {m | isPrivate = b}))
                    ] [],
                    span [] [text "プライベートモード"]
                ],
                div [class "section-title"] [text "出力"],
                div [] [
                    Html.form [
                        method "POST",
                        action model.config.outputUrl,
                        target "_blank"
                    ] [
                        input [type_ "hidden", name "query", value model.outputQuery] [],
                        button [
                            type_ "submit",
                            onMouseDown Output
                        ] [text "Text出力"]
                    ]
                ],
                div [class "section-title"] [text "タグ"],
                div [] [
                    span [] [text "タグは以下に入力し、Enterで追加できます。"]
                ],
                div [] [
                    input [
                        type_ "text",
                        size 10,
                        value model.tagform,
                        onInput (\s -> FormUpdated (\m -> {m | tagform = s})),
                        onKeyPress (\code -> if code == 13 then AddTag else NoOp)
                    ] []
                ],
                div [class "tags"] (List.map (\t -> span [class "tag"] [text t, span [class "ion-close-circled", onClick (RemoveTag t)] []]) model.tags)
            ],
            div [class "right"] 
            [
                ul [class "tabcontrol"] (tabcontorls model),
                div [class "tabbody"] [
                    case model.activeTab of
                        ProfileTab -> profileTab model model.result
                        FavorsTab -> favorsTab model
                        ClassesTab -> classesTab model.classes model.result
                        OtherTab tab -> otherTab tab model.windowSize model.result
                ]
            ]
        ]
    ] ++ (case model.showDeleteTabialog of
        Just tab -> [createDialog "このタブを削除してもよろしいですか？" tab model.windowSize]
        Nothing -> []
    ))

onSubmitDefaultOpt : msg -> Attribute msg
onSubmitDefaultOpt message =
  onWithOptions "submit" defaultOptions (Json.Decode.succeed message)

createResultArea : Maybe ResultMessage -> Html Msg
createResultArea result =
    case result of
        Just (Ok message) ->
            div [class "success"] (
                ([
                    div [style [("text-align", "right")]] [
                        span [
                            class "ion-close-circled",
                            onClick HideResult
                        ] []
                    ]
                ] ++ List.map (\m -> div [] [text m]) message) 
            )
        Just (Err errMessage) ->
            div [class "error"] (
                ([
                    div [style [("text-align", "right")]] [
                        span [
                            class "ion-close-circled",
                            onClick HideResult
                        ] []
                    ]
                ] ++ List.map (\m -> div [] [text m]) errMessage) 
            )
        Nothing -> div [] []

createDialog : String -> Tab -> DomSize -> Html Msg
createDialog content tab windowSize =
    div [] [
        div [class "mask", style [("width", String.append (toString windowSize.width) "px"), ("height", String.append (toString windowSize.height) "px")]] [],
        div [class "dialog", style [("top", "10px"), ("left", String.append (toString (windowSize.width / 2 - 100)) "px")]] [
            div [class "dialog-content"] [
                span [] [text content]
            ],
            div [] [
                button [
                    type_ "button",
                    onClickNoBubble (CloseDialog (Just tab))
                ] [text "OK"],
                button [
                    type_ "button",
                    onClickNoBubble (CloseDialog Nothing)
                ] [text "Cancel"]
            ]
        ]        
    ]

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  on "keypress" (Json.Decode.map tagger keyCode)

onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "change" (Json.Decode.map tagger targetValue)

profileTab : Model -> Maybe ResultMessage -> Html Msg
profileTab model result =
    let
        profile = model.profile
        tabs = model.tabs
    in
        div [] [
            createResultArea result,
            div [class "section-title"] [text "パーソナル"],
            table [] [
                tbody [] [
                    tr [] [
                        th [colspan 2] [text "キャラクター名"],
                        td [colspan 4] [
                            input [size 55, type_ "text", value profile.name, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | name = s}}))] []
                        ]
                    ],
                    tr [] [
                        th [] [text "種族"],
                        td [] [
                            input [size 16, type_ "text", value profile.race, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | race = s}}))] []
                        ],
                        th [] [text "享年"],
                        td [] [
                            input [size 16, type_ "text", value profile.age, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | age = s}}))] []
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
                            input [size 16, type_ "text", value profile.height, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | height = s}}))] []
                        ],
                        th [] [text "体重"],
                        td [] [
                            input [size 16, type_ "text", value profile.weight, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | weight = s}}))] []
                        ],
                        th [] [text "暗示"],
                        td [] [
                            input [size 16, type_ "text", value profile.implication, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | implication = s}}))] []
                        ]
                    ],
                    tr [] [
                        th [] [text "髪の色"],
                        td [] [
                            input [size 16, type_ "text", value profile.hair, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | hair = s}}))] []
                        ],
                        th [] [text "瞳の色"],
                        td [] [
                            input [size 16, type_ "text", value profile.eye, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | eye = s}}))] []
                        ],
                        th [] [text "肌の色"],
                        td [] [
                            input [size 16, type_ "text", value profile.skin, onChange (\s -> FormUpdated (\m -> {m | profile = {profile | skin = s}}))] []
                        ]
                    ],
                    tr [] [
                        th [] [text "最大行動値"],
                        td [] [
                            input [
                                type_ "number",
                                value (toString (getManeuvaAct tabs)),
                                readonly True
                            ] []
                        ],
                        td [colspan 4] []
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
                                onChange (\s -> FormUpdated (\m -> {m | profile = {profile | memories = Utils.updateOnWay profile.memories memory (\mem -> {mem | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value memory.description,
                                onChange (\s -> FormUpdated (\m -> {m | profile = {profile | memories = Utils.updateOnWay profile.memories memory (\mem -> {mem | description = s})}}))
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
                                    onChange (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | target = s})}}))
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value regret.name,
                                    onChange (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | name = s})}}))
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    Html.Attributes.min "0",
                                    Html.Attributes.max (toString regret.maxVal),
                                    value (toString regret.currentVal),
                                    onChange (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | currentVal = Result.withDefault 0 (String.toInt s)})}}))
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    Html.Attributes.min "4",
                                    value (toString regret.maxVal),
                                    onChange (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | maxVal = Result.withDefault 0 (String.toInt s)})}}))
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value regret.negative,
                                    onChange (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | negative = s})}}))
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "text",
                                    value regret.description,
                                    onChange (\s -> FormUpdated (\m -> {m | profile = {profile | regrets = Utils.updateOnWay profile.regrets regret (\reg -> {reg | description = s})}}))
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
                                onChange (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | name = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value karma.description,
                                onChange (\s -> FormUpdated (\m -> {m | profile = {profile | karmas = Utils.updateOnWay profile.karmas karma (\kar -> {kar | description = s})}}))
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
            createResultArea model.result,
            div [class "section-title"] [text "寵愛点サマリー"],
            table [] [
                tbody [] [
                    tr [] [
                        th [] [text "獲得寵愛点"],
                        th [] [text "消費寵愛点"],
                        th [] [text "悪意合計"]
                    ],
                    tr [] [
                        td [] [
                            input [
                                type_ "number",
                                value (toString (List.sum (List.map (\f -> (Maybe.withDefault 0 f.personal) + (Maybe.withDefault 0 f.battle)) favors))),
                                readonly True
                            ] []
                        ],
                        td [] [
                            let
                                parts = getPartFavor tabs
                                skills = getSkillFavor tabs
                                resources = getResourceFavor tabs
                                points = getPointFavor model
                                others = getOtherFavor model
                                usedFavors = getUsedFavor model.usedFavors
                            in
                                input [
                                    type_ "number",
                                    value (toString (parts + skills + resources + points + others + usedFavors)),
                                    readonly True
                                ] []
                        ],
                        td [] [
                            let
                                malice = getMalice tabs
                            in
                                input [
                                    type_ "number",
                                    value (toString malice),
                                    readonly True
                                ] []
                        ]
                    ]
                ]
            ],
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
                                onChange (\s -> FormUpdated (\m -> {m | favors = Utils.updateOnWay favors favor (\x -> {favor | memo = s})}))
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
                                    value (toString (getSkillFavor tabs)),
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
                            td [] [
                                span [class "ion-locked"] []
                            ]
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
                                    value (toString (getPartFavor tabs)),
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
                            td [] [
                                span [class "ion-locked"] []
                            ]
                        ],
                        tr [] [
                            td [] [
                                input [
                                    type_ "text",
                                    value "リソースその他",
                                    readonly True
                                ] []
                            ],
                            td [] [
                                input [
                                    type_ "number",
                                    value (toString (getResourceFavor tabs)),
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
                            td [] [
                                span [class "ion-locked"] []
                            ]
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
                                    value (toString (getPointFavor model)),
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
                            td [] [
                                span [class "ion-locked"] []
                            ]
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
                                    value (toString (getOtherFavor model)),
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
                            td [] [
                                span [class "ion-locked"] []
                            ]
                        ]
                    ] ++ (List.map (\used -> tr [] [
                        td [] [
                            input [
                                type_ "text",
                                value used.purpose,
                                onChange (\s -> FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay usedFavors used (\x -> {used | purpose = s})}))
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
                                onChange (\s -> FormUpdated (\m -> {m | usedFavors = Utils.updateOnWay usedFavors used (\x -> {used | memo = s})}))
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

getMalice : List Tab -> Float
getMalice tabs =
    let
        getMalice = \x -> case x.tabType of
            ManeuvaTab tabData -> List.map 
                (\item -> Maybe.withDefault 0.0 item.malice) tabData.maneuvas
            _ -> []
        total = List.sum (flatMap getMalice tabs)
    in
        List.sum (flatMap getMalice tabs)

getSkillFavor : List Tab -> Int
getSkillFavor tabs =
    let
        getFevors = \x -> case x.tabType of
            ManeuvaTab tabData -> List.map 
                (\item -> case item.maneuvaType of
                    Skill -> Maybe.withDefault 0 item.favor
                    _ -> 0
                ) tabData.maneuvas
            _ -> []
        total = List.sum (flatMap getFevors tabs)
    in
        List.sum (flatMap getFevors tabs)

getPartFavor : List Tab -> Int
getPartFavor tabs =
    let
        getFevors = \x -> case x.tabType of
            ManeuvaTab tabData -> List.map 
                (\item -> case item.maneuvaType of
                    Part -> Maybe.withDefault 0 item.favor
                    _ -> 0
                ) tabData.maneuvas
            _ -> []
        total = List.sum (flatMap getFevors tabs)
    in
        List.sum (flatMap getFevors tabs)

getResourceFavor : List Tab -> Int
getResourceFavor tabs =
    let
        getFevors = \x -> case x.tabType of
            ResourceTab resources -> List.map (\resource -> Maybe.withDefault 0 resource.favor) resources
            _ -> []
        total = List.sum (flatMap getFevors tabs)
    in
        List.sum (flatMap getFevors tabs)

getPointFavor : Model -> Int
getPointFavor model =
    List.sum (List.map (\x -> Maybe.withDefault 0 x.favor) model.classes.points)

getOtherFavor : Model -> Int
getOtherFavor model =
    List.sum (List.map (\x -> Maybe.withDefault 0 x.favor) model.classes.highTechs)

getUsedFavor : List UsedFavor -> Int
getUsedFavor usedFavors =
    List.sum (List.map .favor usedFavors)

getManeuvaAct : List Tab -> Int
getManeuvaAct tabs =
    let
        getActs = \x -> case x.tabType of
            ManeuvaTab tabData -> List.map (\item -> Maybe.withDefault 0 item.act) tabData.maneuvas
            _ -> []
    in
        List.sum (flatMap getActs tabs) + 6

classesTab : Classes -> Maybe ResultMessage -> Html Msg
classesTab classes result =
    div [] [
        createResultArea result,
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
                                onChange (\s -> FormUpdated (\m -> {m | classes = {classes | positions = Utils.updateOnWay classes.positions position (\x -> {position | name = s})}}))
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
                                onChange (\s -> FormUpdated (\m -> {m | classes = {classes | subPositions = Utils.updateOnWay classes.subPositions subPosition (\x -> {subPosition | name = s})}}))
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
                                onChange (\s -> FormUpdated (\m -> {m | classes = {classes | highTechs = Utils.updateOnWay classes.highTechs highTech (\x -> {highTech | name = s})}}))
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
                                onChange (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | from = s})}}))
                            ] []
                        ],
                        td [] [
                            input [
                                type_ "text",
                                value clazz.name,
                                onChange (\s -> FormUpdated (\m -> {m | classes = {classes | classes = Utils.updateOnWay classes.classes clazz (\cls -> {cls | name = s})}}))
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
                                onChange (\s -> FormUpdated (\m -> {m | classes = {classes | points = Utils.updateOnWay classes.points point (\x -> {point | name = s})}}))
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
                        td [] [
                            span [class "ion-locked"] []
                        ]
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

otherTab : Tab -> DomSize -> Maybe ResultMessage -> Html Msg
otherTab tab windowSize result =
    div [
        id tab.uuid
    ] [
        createResultArea result,
        div [class "section-title"] [text tab.title],
        let
            tabbody =
                case tab.tabType of
                    ManeuvaTab _ -> maneuvaTab tab windowSize
                    ResourceTab _ -> resourceTab tab
                    -- TODO タブ種類が増えた時にここに追加する
        in
            tabbody
    ]

resourceTab : Tab -> Html Msg
resourceTab tab =
    let
        resources = case tab.tabType of
            ResourceTab resources -> resources
            _ -> []
    in
        div [] [
            table [] [
                tbody [] ([
                    tr [] [
                        th [] [text "No."],
                        th [] [text "名前"],
                        th [] [text "説明"],
                        th [] [text "寵愛"],
                        td [] [],
                        td [] []
                    ]
                ] ++ (List.map (\resource ->
                    let
                        index = case resource.position of
                            Position index -> index
                    in
                        tr (
                            (DragDrop.droppable RowDragDrop resource.position) ++ 
                            (DragDrop.draggable RowDragDrop resource.uuid) ++
                            [
                                id resource.uuid
                            ]
                        )
                        [
                            th [] [text (toString (index + 1))],
                            td [] [
                                let
                                    val = resource.name
                                in
                                    input [
                                        type_ "text",
                                        value val,
                                        onChange (\s -> FormUpdated (\m ->
                                        let 
                                            newTabState = {tab | tabType = case tab.tabType of
                                                ResourceTab resources -> ResourceTab (Utils.updateOnWay resources resource (\x -> 
                                                {x | name = s}))
                                                _ -> tab.tabType
                                            }
                                        in
                                            {m |
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                                activeTab = OtherTab newTabState
                                            }
                                        ))
                                    ] []
                            ],
                            td [] [
                                let
                                    val = resource.description
                                in
                                    input [
                                        type_ "text",
                                        value val,
                                        onChange (\s -> FormUpdated (\m ->
                                        let 
                                            newTabState = {tab | tabType = case tab.tabType of
                                                ResourceTab resources -> ResourceTab (Utils.updateOnWay resources resource (\x -> 
                                                {x | description = s}))
                                                _ -> tab.tabType
                                            }
                                        in
                                            {m |
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                                activeTab = OtherTab newTabState
                                            }
                                        ))
                                    ] []
                            ],
                            td [] [
                                let
                                    val = case resource.favor of
                                        Just favor -> toString favor
                                        Nothing -> ""
                                in
                                    input [
                                        class "num",
                                        type_ "number",
                                        value val,
                                        Html.Attributes.min "0",
                                        onInput (\s -> FormUpdated (\m ->
                                        let 
                                            newTabState = {tab | tabType = case tab.tabType of
                                                ResourceTab resources -> ResourceTab (Utils.updateOnWay resources resource (\x -> {x | favor = 
                                                    case String.toInt s of
                                                        Ok num -> Just num
                                                        _ -> Nothing
                                                }))
                                                _ -> tab.tabType
                                            }
                                        in
                                            {m |
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                                activeTab = OtherTab newTabState
                                            }
                                        ))
                                    ] []
                            ],
                            td [] [
                                if index == 0 then
                                    span [
                                        class "ion-locked"
                                    ] []
                                else
                                    span [
                                        class "ion-close-round",
                                        let
                                            eq = \x -> x == resource

                                            prevPosition = \resource ->
                                                case resource.position of
                                                    Position index -> Position (index - 1)

                                            newTabState = {tab | tabType = case tab.tabType of
                                                ResourceTab resources -> ResourceTab ((Utils.takeNotWhile eq resources) ++ (List.map (\r -> {r | position = prevPosition r}) (Utils.dropNotWhile eq resources)))
                                                _ -> tab.tabType
                                            }
                                        in
                                            onClick (RemoveRow (\m -> {m |
                                                activeTab = OtherTab newTabState,
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState)
                                            }))
                                    ] []
                            ],
                            td [] [
                                span [
                                    class "ion-plus-round",
                                    let
                                        eq = \x -> x == resource
                                    in
                                        onClick (AddRow 
                                            (\model -> 
                                                generate (\uuid -> FormUpdated (\m -> 
                                                    let
                                                        nextPosition = \item ->
                                                            case item.position of
                                                                Position index -> Position (index + 1)
                                                        newTabState = {tab | tabType = case tab.tabType of
                                                            ResourceTab resources -> ResourceTab ((Utils.takeNotWhile eq resources) ++ [
                                                                resource,
                                                                {
                                                                    uuid = uuid, 
                                                                    name = "",
                                                                    description = "",
                                                                    favor = Nothing,
                                                                    position = nextPosition resource
                                                                }
                                                            ] ++(List.map (\x -> {x | position = nextPosition x}) (Utils.dropNotWhile eq resources)))
                                                            _ -> tab.tabType
                                                        }
                                                    in
                                                    {m | 
                                                        activeTab = OtherTab newTabState,
                                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState)
                                                    }
                                                )
                                                ) uuidStringGenerator
                                            )
                                        )
                                ] []
                            ]
                        ]
                )) (List.sortBy (\r -> case r.position of
                    Position index -> index
                ) resources))
            ]
        ]


maneuvaTab : Tab -> DomSize -> Html Msg
maneuvaTab tab windowSize =
    let
        (items, showAddManeuvaDialog) = case tab.tabType of
            ManeuvaTab {maneuvas, showAddManeuvaDialog} -> (maneuvas, showAddManeuvaDialog)
            _ -> ([], False)
        
        tabF = if tab.isLimited then maneuvaLimitedTab else maneuvaFullTab

        itemSize = List.length items

        maxPageNum = itemSize // 100

        currentPage = tab.currentPage

        makePagenationItem num =
            if tab.currentPage == num then
                span [class "page current-page"] [text (toString (num + 1))]
            else
                span [class "page", onClick (PageChange tab num)] [text (toString (num + 1))]

        limitedList = List.take 100 (List.drop (currentPage * 100) items)
    in
        div [] (
            (
                if List.isEmpty tab.mismatches |> not then [
                    div [class "mismatch"] (
                        [
                            div [style [("text-align", "right")]] [
                                span [
                                    class "ion-close-circled",
                                    onClick (ResetMessages tab)
                                ] []
                            ],
                            div [] [text "以下のマニューバのインポートに失敗しました。"]
                        ] ++
                        List.map (\mis -> 
                            div [class "indent-1"] [text mis]
                        ) tab.mismatches
                    )
                ] else []
            ) ++ [
                div [] [
                    button [
                        onClick (OpenManeuvaDialog tab)
                    ] [text "マニューバをインポートする"],
                    button [
                        onClick (ToggleManeuvaTab tab)
                    ] [text "表示モードを切り替える"]
                ],
                div [class "pagenation"] (List.map makePagenationItem (List.range 0 maxPageNum))
            ]++ (tabF limitedList tab) ++
            [
                div [class "pagenation"] (List.map makePagenationItem (List.range 0 maxPageNum)),
                div [] [
                    button [
                        onClick (OpenManeuvaDialog tab)
                    ] [text "マニューバをインポートする"],
                    button [
                        onClick (ToggleManeuvaTab tab)
                    ] [text "表示モードを切り替える"]
                ]
            ] ++
            if showAddManeuvaDialog then 
                [
                    createManeuvaDialog tab windowSize
                ]
            else
                []
        )

maneuvaLimitedTab : List Maneuva -> Tab -> List (Html Msg)
maneuvaLimitedTab items tab =
    [
        table [] [
            tbody [] ([
                tr [] [
                    th [] [text "No."],
                    th [] [text "カテゴリー"],
                    th [] [text "部位"],
                    th [] [text "マニューバ"],
                    th [] [text "タイミング"],
                    th [] [text "コスト"],
                    th [] [text "射程"],
                    th [] [text "効果"],
                    td [] [],
                    td [] []
                ]
            ] ++ (List.map (\item -> 
            let
                index = case item.position of
                    Position index -> index
            in
                tr (
                    (DragDrop.droppable RowDragDrop item.position) ++ 
                    (DragDrop.draggable RowDragDrop item.uuid) ++
                    [
                        id item.uuid
                    ]
                )
                [
                    th [] [text (toString (index + 1))],
                    td [] [
                        let
                            val =
                                case item.maneuvaType of
                                    Skill -> "0"
                                    Part -> "1"
                                    Item -> "2"
                                    Effect -> "3"
                                    Archive -> "4"
                                    Tag -> "5"
                        in
                            select [
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | maneuvaType = case s of 
                                                        "0" -> Skill
                                                        "1" -> Part
                                                        "2" -> Item
                                                        "3" -> Effect
                                                        "4" -> Archive
                                                        "5" -> Tag
                                                        _ -> Part
                                                    }
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "スキル"],
                                option [value "1", selected (val == "1")] [text "パーツ"],
                                option [value "2", selected (val == "2")] [text "アイテム"],
                                option [value "3", selected (val == "3")] [text "エフェクト"],
                                option [value "4", selected (val == "4")] [text "アーカイブ"],
                                option [value "5", selected (val == "5")] [text "タグ"]
                            ]
                    ],
                    td [] [
                        let
                            val =
                                case item.region of
                                    NoRegion -> "0"
                                    Head -> "1"
                                    Arm -> "2"
                                    Body -> "3"
                                    Leg -> "4"
                                    Girl -> "6"
                                    OtherRegion -> "5"
                        in
                            select [
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | region = case s of 
                                                        "0" -> NoRegion
                                                        "1" -> Head
                                                        "2" -> Arm
                                                        "3" -> Body
                                                        "4" -> Leg
                                                        "6" -> Girl
                                                        "5" -> OtherRegion
                                                        _ -> NoRegion
                                                    }
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "なし"],
                                option [value "1", selected (val == "1")] [text "頭"],
                                option [value "2", selected (val == "2")] [text "腕"],
                                option [value "3", selected (val == "3")] [text "胴"],
                                option [value "4", selected (val == "4")] [text "脚"],
                                option [value "5", selected (val == "5")] [text "その他"],
                                option [value "6", selected (val == "6")] [text "少女"]
                            ]
                    ],
                    td [] [
                        let
                            val = item.name
                        in
                            input [
                                type_ "text",
                                value val,
                                size 30,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | name = s}
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item.timing of
                                    AutoAlways -> "0"
                                    AutoNeedsDeclearation -> "1"
                                    AutoOthers -> "2"
                                    Action -> "3"
                                    Judge -> "4"
                                    Damage -> "5"
                                    Rapid -> "6"
                                    BeforeBattle -> "7"
                                    BattleStart -> "8"
                                    TurnStart -> "9"
                                    CountStart -> "10"
                        in
                            select [
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | timing = case s of
                                                        "0" -> AutoAlways
                                                        "1" -> AutoNeedsDeclearation
                                                        "2" -> AutoOthers
                                                        "3" -> Action
                                                        "4" -> Judge
                                                        "5" -> Damage
                                                        "6" -> Rapid
                                                        "7" -> BeforeBattle
                                                        "8" -> BattleStart
                                                        "9" -> TurnStart
                                                        "10" -> CountStart
                                                        _ -> AutoAlways
                                                    }
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
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
                                option [value "6", selected (val == "6")] [text "ラピッド"],
                                option [value "7", selected (val == "7")] [text "戦闘開始前"],
                                option [value "8", selected (val == "8")] [text "戦闘開始時"],
                                option [value "9", selected (val == "9")] [text "ターン開始"],
                                option [value "10", selected (val == "10")] [text "カウント開始"]
                            ]
                    ],
                    td [] [
                        let
                            val = item.cost
                        in
                            input [
                                type_ "text",
                                value val,
                                size 4,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | cost = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val = item.range
                        in
                            input [
                                type_ "text",
                                value val,
                                size 4,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | range = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val = item.description
                        in
                            input [
                                type_ "text",
                                value val,
                                size 80,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | description = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        if index == 0 then
                            span [
                                class "ion-locked"
                            ] []
                        else
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == item

                                    prevPosition = \item ->
                                        case item.position of
                                            Position index -> Position (index - 1)

                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> ManeuvaTab {
                                            tabData |
                                                maneuvas = 
                                                (
                                                        (Utils.takeNotWhile eq tabData.maneuvas) ++
                                                        (List.map (\i -> {i | position = prevPosition(i)}) (Utils.dropNotWhile eq tabData.maneuvas))
                                                )
                                        }
                                        _ -> tab.tabType
                                    }
                                in
                                    onClick (RemoveRow (\m -> {m |
                                        activeTab = OtherTab newTabState,
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState)
                                    }))
                            ] []
                    ],
                    td [] [
                        span [
                            class "ion-plus-round",
                            let
                                eq = \x -> x == item
                                newTabState = {tab | tabType = case tab.tabType of
                                    ManeuvaTab tabData -> ManeuvaTab 
                                        {
                                            tabData |
                                                maneuvas = ((Utils.takeNotWhile eq items) ++ (Utils.dropNotWhile eq items))
                                        }
                                    _ -> tab.tabType
                                }
                            in
                                onClick (AddRow 
                                    (\model -> 
                                        generate (\uuid -> FormUpdated (\m -> 
                                            let
                                                nextPosition = \item ->
                                                    case item.position of
                                                        Position index -> Position (index + 1)
                                                newTabState = {tab | tabType = case tab.tabType of
                                                    ManeuvaTab tabData -> ManeuvaTab {
                                                        tabData |
                                                            maneuvas = 
                                                                ((Utils.takeNotWhile eq tabData.maneuvas) ++ [
                                                                        item,
                                                                        {
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
                                                                            from = "",
                                                                            region = Head,
                                                                            maneuvaType = Skill,
                                                                            position = nextPosition(item)
                                                                        }
                                                                    ] ++ (List.map (\x -> {x | position = nextPosition x}) (Utils.dropNotWhile eq tabData.maneuvas)))
                                                    }
                                                    _ -> tab.tabType
                                                }
                                            in
                                            {m | 
                                                activeTab = OtherTab newTabState,
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState)
                                            }
                                        )
                                        ) uuidStringGenerator
                                    )
                                )
                        ] []
                    ]
                ]
            ) (List.sortBy (\r -> case r.position of
                Position index -> index
            ) items)))
        ]
    ]

maneuvaFullTab : List Maneuva -> Tab -> List (Html Msg)
maneuvaFullTab items tab =
    [
        table [] [
            tbody [] ([
                tr [] [
                    th [] [text "No."],
                    th [] [text "損傷"],
                    th [] [text "使用"],
                    th [] [text "悪意"],
                    th [] [text "行動値"],
                    th [] [text "カテゴリー"],
                    th [] [text "部位"],
                    th [] [text "種別"],
                    th [] [text "マニューバ"],
                    th [] [text "タイミング"],
                    th [] [text "コスト"],
                    th [] [text "射程"],
                    th [] [text "効果"],
                    th [] [text "取得先"],
                    th [] [text "寵愛"],
                    td [] [],
                    td [] []
                ]
            ] ++ (List.map (\item -> 
            let
                index = case item.position of
                    Position index -> index
            in
                tr (
                    (DragDrop.droppable RowDragDrop item.position) ++ 
                    (DragDrop.draggable RowDragDrop item.uuid) ++
                    [
                        id item.uuid
                    ]
                )
                [
                    th [] [text (toString (index + 1))],
                    th [] [
                        let
                            boxChecked = item.lost
                        in
                            input [
                                class "check",
                                type_ "checkbox",
                                checked boxChecked,
                                onCheck (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> {x | lost = s})
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    th [] [
                        let
                            boxChecked = item.used
                        in
                            input [
                                class "check",
                                type_ "checkbox",
                                checked boxChecked,
                                onCheck (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> {x | used = s})
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val = 
                                case item.malice of
                                    Just malice -> toString malice
                                    Nothing -> ""
                        in
                            input [
                                type_ "text",
                                size 4,
                                value val,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> {x | malice = 
                                                    case String.toFloat s of
                                                        Ok num -> Just num
                                                        _ -> Nothing})
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item.act of
                                    Just act -> toString act
                                    Nothing -> ""
                        in
                            input [
                                class "num",
                                type_ "number",
                                value val,
                                Html.Attributes.min "0",
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> {x | act = 
                                                    case String.toInt s of
                                                        Ok num -> Just num
                                                        _ -> Nothing})
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item.maneuvaType of
                                    Skill -> "0"
                                    Part -> "1"
                                    Item -> "2"
                                    Effect -> "3"
                                    Archive -> "4"
                                    Tag -> "5"
                        in
                            select [
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | maneuvaType = case s of 
                                                        "0" -> Skill
                                                        "1" -> Part
                                                        "2" -> Item
                                                        "3" -> Effect
                                                        "4" -> Archive
                                                        "5" -> Tag
                                                        _ -> Part
                                                    }
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "スキル"],
                                option [value "1", selected (val == "1")] [text "パーツ"],
                                option [value "2", selected (val == "2")] [text "アイテム"],
                                option [value "3", selected (val == "3")] [text "エフェクト"],
                                option [value "4", selected (val == "4")] [text "アーカイブ"],
                                option [value "5", selected (val == "5")] [text "タグ"]
                            ]
                    ],
                    td [] [
                        let
                            val =
                                case item.region of
                                    NoRegion -> "0"
                                    Head -> "1"
                                    Arm -> "2"
                                    Body -> "3"
                                    Leg -> "4"
                                    Girl -> "6"
                                    OtherRegion -> "5"
                        in
                            select [
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | region = case s of 
                                                        "0" -> NoRegion
                                                        "1" -> Head
                                                        "2" -> Arm
                                                        "3" -> Body
                                                        "4" -> Leg
                                                        "6" -> Girl
                                                        "5" -> OtherRegion
                                                        _ -> NoRegion
                                                    }
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "なし"],
                                option [value "1", selected (val == "1")] [text "頭"],
                                option [value "2", selected (val == "2")] [text "腕"],
                                option [value "3", selected (val == "3")] [text "胴"],
                                option [value "4", selected (val == "4")] [text "脚"],
                                option [value "5", selected (val == "5")] [text "その他"],
                                option [value "6", selected (val == "6")] [text "少女"]
                            ]
                    ],
                    td [] [
                        let
                            val = item.category
                        in
                            select [
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | category = s}
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] [
                                option [value "0", selected (val == "0")] [text "通常技"],
                                option [value "1", selected (val == "1")] [text "必殺技"],
                                option [value "2", selected (val == "2")] [text "行動値"],
                                option [value "3", selected (val == "3")] [text "支援"],
                                option [value "4", selected (val == "4")] [text "妨害"],
                                option [value "5", selected (val == "5")] [text "防御"],
                                option [value "6", selected (val == "6")] [text "移動"],
                                option [value "7", selected (val == "7")] [text "打ち消し"],
                                option [value "8", selected (val == "8")] [text "無効"],
                                option [value "9", selected (val == "9")] [text "効果なし"],
                                option [value "10", selected (val == "10")] [text "耐性"],
                                option [value "11", selected (val == "11")] [text "再使用"],
                                option [value "12", selected (val == "12")] [text "対話"],
                                option [value "13", selected (val == "13")] [text "行動出目"],
                                option [value "14", selected (val == "14")] [text "攻撃出目"]
                            ]
                    ],
                    td [] [
                        let
                            val = item.name
                        in
                            input [
                                type_ "text",
                                value val,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | name = s}
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item.timing of
                                    AutoAlways -> "0"
                                    AutoNeedsDeclearation -> "1"
                                    AutoOthers -> "2"
                                    Action -> "3"
                                    Judge -> "4"
                                    Damage -> "5"
                                    Rapid -> "6"
                                    BeforeBattle -> "7"
                                    BattleStart -> "8"
                                    TurnStart -> "9"
                                    CountStart -> "10"
                        in
                            select [
                                value val,
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData ->
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | timing = case s of
                                                        "0" -> AutoAlways
                                                        "1" -> AutoNeedsDeclearation
                                                        "2" -> AutoOthers
                                                        "3" -> Action
                                                        "4" -> Judge
                                                        "5" -> Damage
                                                        "6" -> Rapid
                                                        "7" -> BeforeBattle
                                                        "8" -> BattleStart
                                                        "9" -> TurnStart
                                                        "10" -> CountStart
                                                        _ -> AutoAlways
                                                    }
                                                )                                                       
                                            }
                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
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
                                option [value "6", selected (val == "6")] [text "ラピッド"],
                                option [value "7", selected (val == "7")] [text "戦闘開始前"],
                                option [value "8", selected (val == "8")] [text "戦闘開始時"],
                                option [value "9", selected (val == "9")] [text "ターン開始"],
                                option [value "10", selected (val == "10")] [text "カウント開始"]
                            ]
                    ],
                    td [] [
                        let
                            val = item.cost
                        in
                            input [
                                type_ "text",
                                value val,
                                size 4,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | cost = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val = item.range
                        in
                            input [
                                type_ "text",
                                value val,
                                size 4,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | range = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val = item.description
                        in
                            input [
                                type_ "text",
                                value val,
                                size 30,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | description = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val = item.from
                        in
                            input [
                                type_ "text",
                                value val,
                                size 10,
                                onChange (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | from = s}
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        let
                            val =
                                case item.favor of
                                    Just favor -> toString favor
                                    Nothing -> ""
                        in
                            input [
                                class "num",
                                type_ "number",
                                value val,
                                Html.Attributes.min "0",
                                onInput (\s -> FormUpdated (\m ->
                                let 
                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> 
                                            ManeuvaTab {tabData |
                                                maneuvas = Utils.updateOnWay tabData.maneuvas item (\x -> 
                                                    {x | favor = 
                                                        case String.toInt s of
                                                            Ok num -> Just num
                                                            _ -> Nothing
                                                    }
                                                )                                                       
                                            }

                                        _ -> tab.tabType
                                    }
                                in
                                    {m |
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                                        activeTab = OtherTab newTabState
                                    }
                                ))
                            ] []
                    ],
                    td [] [
                        if index == 0 then
                            span [
                                class "ion-locked"
                            ] []
                        else
                            span [
                                class "ion-close-round",
                                let
                                    eq = \x -> x == item

                                    prevPosition = \item ->
                                        case item.position of
                                            Position index -> Position (index - 1)

                                    newTabState = {tab | tabType = case tab.tabType of
                                        ManeuvaTab tabData -> ManeuvaTab {
                                            tabData |
                                                maneuvas = 
                                                (
                                                        (Utils.takeNotWhile eq tabData.maneuvas) ++
                                                        (List.map (\i -> {i | position = prevPosition(i)}) (Utils.dropNotWhile eq tabData.maneuvas))
                                                )
                                        }
                                        _ -> tab.tabType
                                    }
                                in
                                    onClick (RemoveRow (\m -> {m |
                                        activeTab = OtherTab newTabState,
                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState)
                                    }))
                            ] []
                    ],
                    td [] [
                        span [
                            class "ion-plus-round",
                            let
                                eq = \x -> x == item
                                newTabState = {tab | tabType = case tab.tabType of
                                    ManeuvaTab tabData -> ManeuvaTab 
                                        {
                                            tabData |
                                                maneuvas = ((Utils.takeNotWhile eq items) ++ (Utils.dropNotWhile eq items))
                                        }
                                    _ -> tab.tabType
                                }
                            in
                                onClick (AddRow 
                                    (\model -> 
                                        generate (\uuid -> FormUpdated (\m -> 
                                            let
                                                nextPosition = \item ->
                                                    case item.position of
                                                        Position index -> Position (index + 1)
                                                newTabState = {tab | tabType = case tab.tabType of
                                                    ManeuvaTab tabData -> ManeuvaTab {
                                                        tabData |
                                                            maneuvas = 
                                                                ((Utils.takeNotWhile eq tabData.maneuvas) ++ [
                                                                        item,
                                                                        {
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
                                                                            from = "",
                                                                            region = Head,
                                                                            maneuvaType = Skill,
                                                                            position = nextPosition(item)
                                                                        }
                                                                    ] ++ (List.map (\x -> {x | position = nextPosition x}) (Utils.dropNotWhile eq tabData.maneuvas)))
                                                    }
                                                    _ -> tab.tabType
                                                }
                                            in
                                            {m | 
                                                activeTab = OtherTab newTabState,
                                                tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState)
                                            }
                                        )
                                        ) uuidStringGenerator
                                    )
                                )
                        ] []
                    ]
                ]
            ) (List.sortBy (\r -> case r.position of
                Position index -> index
            ) items)))
        ]
    ]

createManeuvaDialog : Tab -> DomSize -> Html Msg
createManeuvaDialog tab windowSize =
    div [] [
        div [class "mask", style [("width", String.append (toString windowSize.width) "px"), ("height", String.append (toString windowSize.height) "px")]] [],
        div [class "dialog", style [("top", "10px"), ("left", String.append (toString (windowSize.width / 2 - 300)) "px")]] [
            div [class "dialog-title"] [
                text "マニューバ追加"
            ],
            hr [] [],
            div [class "dialog-content"] [
                div [] [text "以下のテキストボックスにマニューバのテキストを貼り付けてください。"],
                div [] [text "複数貼り付ける場合は、改行で区切って下さい。"],
                textarea [
                    cols 100,
                    rows 10,
                    onChange (\s -> FormUpdated (\m ->
                    let 
                        newTabState = {tab | tabType = case tab.tabType of
                            ManeuvaTab tabData -> 
                                ManeuvaTab {tabData |
                                    dialogContent = if String.isEmpty s then Nothing else Just s
                                }
                            _ -> tab.tabType
                        }
                    in
                        {m |
                            tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb -> newTabState),
                            activeTab = OtherTab newTabState
                        }
                    ))
                ] []
            ],
            div [] [
                button [
                    type_ "button",
                    onClickNoBubble (CloseManeuvaDialog tab)
                ] [text "OK"],
                button [
                    type_ "button",
                    onClickNoBubble (CancelManeuvaDialog tab)
                ] [text "Cancel"]
            ]
        ]        
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
                    input [
                        type_ "text",
                        value currentTab.title,
                        size 10,
                        onChange (\s -> FormUpdated (\m ->
                            let 
                                newTabState = {currentTab | title = s}
                            in
                                {m |
                                    tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == currentTab.uuid) currentTab (\tb -> newTabState),
                                    activeTab = OtherTab newTabState
                                }
                            ))
                    ] [] else
                    text currentTab.title
            ],
            span [
                class "ion-close-round",
                onClickNoBubble (OpenDialog currentTab)
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
                select [
                    class "new-tab-type",
                    onInput (\s -> FormUpdated (\m -> {m | appendMode = case s of
                        "maneuva" -> AppendManeuva
                        "resource" -> AppendResource
                        _ -> AppendResource
                    }))
                ] [
                    option [value "maneuva", selected (
                            case model.appendMode of
                                AppendManeuva -> True
                                _ -> False
                            )
                    ] [text "マニューバ"],
                    option [value "resource", selected (
                            case model.appendMode of
                                AppendResource -> True
                                _ -> False
                            )
                    ] [text "リソース"]
                ],
                span [
                    class "add-tab",
                    class "ion-plus-round",
                    onClick (AddTab 
                        (\model ->
                            let
                                newManeuva =
                                    {
                                        uuid = "", 
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
                                        from = "",
                                        region = Head,
                                        maneuvaType = Skill,
                                        position = Position 0
                                    }

                                newResource =
                                    {
                                        uuid = "",
                                        name = "",
                                        description = "",
                                        favor = Nothing,
                                        position = Position 0
                                    }

                                tabType = 
                                    case model.appendMode of
                                        AppendManeuva -> ManeuvaTab {
                                                dialogContent = Nothing,
                                                maneuvas = [newManeuva],
                                                showAddManeuvaDialog = False
                                            }
                                        AppendResource -> ResourceTab [newResource]

                                title = 
                                    case model.appendMode of
                                        AppendManeuva -> "マニューバ"
                                        AppendResource -> "リソース"

                                getItems = \tab ->
                                    case tab.tabType of
                                        ManeuvaTab {maneuvas, showAddManeuvaDialog} -> maneuvas
                                        _ -> []

                                getResources = \tab ->
                                    case tab.tabType of
                                        ResourceTab resources -> resources
                                        _ -> []

                                newTabState = {
                                    uuid = "",
                                    tabType = tabType,
                                    title = title,
                                    isEditing = False,
                                    mismatches = [],
                                    isLimited = False,
                                    currentPage = 0
                                }

                                newTabs = model.tabs ++ [newTabState]
                            in
                                Cmd.batch (
                                    [
                                        (
                                            generate (\uuid -> FormUpdated (\m ->
                                                let
                                                    tabs = case List.head (List.reverse m.tabs) of
                                                        Just tab -> Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb ->
                                                            {tb | tabType = 
                                                                case tb.tabType of
                                                                    ManeuvaTab {maneuvas, showAddManeuvaDialog} ->
                                                                        ManeuvaTab {
                                                                            dialogContent = Nothing,
                                                                            maneuvas = 
                                                                                (
                                                                                    let 
                                                                                        newManeuvas =
                                                                                            Utils.updateOnWayUseEq maneuvas (\x -> x.uuid == "") newManeuva (\ma -> {ma | uuid = uuid})
                                                                                    in
                                                                                        newManeuvas
                                                                                ),
                                                                            showAddManeuvaDialog = False
                                                                        }
                                                                    _ -> tb.tabType
                                                            }
                                                        )
                                                        Nothing -> []
                                                in
                                                    {m | 
                                                        tabs = tabs
                                                    }
                                                )
                                            ) uuidStringGenerator
                                        ),
                                        (
                                            generate (\uuid -> FormUpdated (\m ->
                                                let
                                                    tabs = case List.head (List.reverse m.tabs) of
                                                        Just tab -> Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == tab.uuid) tab (\tb ->
                                                            {tb | tabType = 
                                                                case tb.tabType of
                                                                    ResourceTab resources ->
                                                                        ResourceTab (
                                                                            let 
                                                                                newResources =
                                                                                    Utils.updateOnWayUseEq resources (\x -> x.uuid == "") newResource (\ma -> {ma | uuid = uuid})
                                                                            in
                                                                                newResources
                                                                        )
                                                                    _ -> tb.tabType
                                                            }
                                                        )
                                                        Nothing -> []
                                                in
                                                    {m | 
                                                        tabs = tabs
                                                    }
                                                )
                                            ) uuidStringGenerator
                                        ),
                                        (
                                            generate (\uuid -> FormUpdated (\m -> 
                                                    {m | 
                                                        tabs = Utils.updateOnWayUseEq m.tabs (\x -> x.uuid == "") newTabState (\tb -> {tb | uuid = uuid})
                                                    }
                                                )
                                            ) uuidStringGenerator
                                        )
                                    ]
                                )
                        )
                    )
                ] []
            ]
        ]