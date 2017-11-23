module Messages exposing (..)

import Models exposing (..)

type Msg =
    NoOp |
    AddRow (Model -> Cmd Msg) |
    RemoveRow (Model -> Model) |
    ToggleEdit Tab |
    PersonalTabClicked |
    ClassesTabClicked |
    FavorsTabClicked |
    OtherTabClicked Tab |
    FormUpdated (Model -> Model) |
    AddTab (Model -> Cmd Msg) |
    RemoveTab Tab
