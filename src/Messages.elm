module Messages exposing (..)

import Models exposing (..)

type Msg =
    NoOp |
    AddRow (Model -> Cmd Msg) |
    RemoveRow (Model -> Model) |
    PersonalTabClicked |
    ClassesTabClicked |
    FavorsTabClicked |
    OtherTabClicked Tab |
    FormUpdated (Model -> Model)
