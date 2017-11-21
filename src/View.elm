module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (class, value, attribute, selected, type_, size, colspan)
import Messages exposing (Msg(..))
import Models exposing (Model, ActiveTab(..), AppendMode(..), Tab, Profile, Classes, Favor, Place(..))

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
        span [class "section-title"] [text "パーソナル"],
        table [] [
            tbody [] [
                tr [] [
                    th [colspan 2] [text "キャラクター名"],
                    td [colspan 4] [
                        input [size 55, type_ "text", value profile.name] []
                    ]
                ],
                tr [] [
                    th [] [text "種族"],
                    td [] [
                        input [size 16, type_ "text", value profile.race] []
                    ],
                    th [] [text "享年"],
                    td [] [
                        input [size 16, type_ "text", value profile.age] []
                    ],
                    th [] [text "初期配置"],
                    td [] [
                        select [] [
                            option (
                                case profile.place of
                                    Purgatory -> [selected True]
                                    _ -> []
                            ) [text "煉獄"],
                            option (
                                case profile.place of
                                    Garden -> [selected True]
                                    _ -> []
                            ) [text "花園"],
                            option (
                                case profile.place of
                                    Paradice -> [selected True]
                                    _ -> []
                            ) [text "楽園"]
                        ]
                    ]
                ],
                tr [] [
                    th [] [text "身長"],
                    td [] [
                        input [size 16, type_ "text", value profile.height] []
                    ],
                    th [] [text "体重"],
                    td [] [
                        input [size 16, type_ "text", value profile.weight] []
                    ],
                    th [] [text "暗示"],
                    td [] [
                        input [size 16, type_ "text", value profile.implication] []
                    ]
                ],
                tr [] [
                    th [] [text "髪の色"],
                    td [] [
                        input [size 16, type_ "text", value profile.hair] []
                    ],
                    th [] [text "瞳の色"],
                    td [] [
                        input [size 16, type_ "text", value profile.eye] []
                    ],
                    th [] [text "肌の色"],
                    td [] [
                        input [size 16, type_ "text", value profile.skin] []
                    ]
                ]
            ]
        ],
        span [class "section-title"] [text "記憶のカケラ"],
        table [] [
            tbody [] (
                [
                    tr [] [
                        th [] [text "名前"],
                        th [] [text "詳細"]
                    ]
                ] ++ (List.map (\memory -> tr [] [
                    th [] [
                        input [size 20, type_ "text", value memory.name] []
                    ],
                    td [] [
                        input [size 50, type_ "text", value memory.description] []
                    ]
                ]) profile.memories)
            )
        ],
        div [] [
            button [type_ "button", onClick AddMemory] [text "追加"]
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
        classes = [class "tabctl", class "appendable"] ++ case model.activeTab of
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
        personalClass = [class "tabctl"] ++ case model.activeTab of
            ProfileTab -> [class "active"]
            _ -> []
        classesClass = [class "tabctl"] ++ case model.activeTab of
            FavorsTab -> [class "active"]
            _ -> []
        favorsClass = [class "tabctl"] ++ case model.activeTab of
            ClassesTab -> [class "active"]
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