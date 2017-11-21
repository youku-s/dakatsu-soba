module View exposing (..)

import Html exposing (Html, div, text, ul, li, span, select, option, node, body)
import Html.Attributes exposing (class, value, attribute, selected)
import Messages exposing (Msg)
import Models exposing (Model, ActiveTab(..), AppendMode(..), Tab)

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
                ul [class "tabcontrol"] (tabcontorls model)
            ]
        ]
    ]

tabToLi : Model -> Tab -> Html Msg
tabToLi model currentTab =
    let
        classes = [class "tabctl", class "appendable"] ++ case model.activeTab of
                OtherTab active -> if active == currentTab then [class "active"] else []
                _ -> []
    in
        li classes []

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