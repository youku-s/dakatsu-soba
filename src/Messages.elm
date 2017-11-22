module Messages exposing (..)

import Models exposing (..)

type Msg =
    NoOp |
    AddMemory |
    RemoveMemory Memory |
    PersonalTabClicked |
    ClassesTabClicked |
    FavorsTabClicked |
    OtherTabClicked Tab
