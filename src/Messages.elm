module Messages exposing (..)

import Models exposing (..)
import Window exposing (Size)
import Html5.DragDrop as DragDrop

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
    RemoveTab Tab |
    AddTag |
    RemoveTag String |
    OpenDialog Tab |
    CloseDialog (Maybe Tab) |
    SetWindowSize Size |
    RowDragDrop (DragDrop.Msg String Position) |
    OpenManeuvaDialog Tab |
    CloseManeuvaDialog Tab