module Messages exposing (..)

import Models exposing (..)
import Html5.DragDrop as DragDrop
import Navigation exposing (Location)

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
    SetWindowSize DomSize |
    RowDragDrop (DragDrop.Msg String Position) |
    OpenManeuvaDialog Tab |
    CloseManeuvaDialog Tab |
    CancelManeuvaDialog Tab |
    ResetMessages Tab |
    UpdateSize DomSize |
    Save |
    Delete |
    Clone |
    Output |
    OnLocationChange Location |
    LoadDataFromJson String |
    ShowResult ResultMessage |
    HideResult |
    OnLoad (Maybe String) ResultMessage