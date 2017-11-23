module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (..)
import Utils exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        AddRow newCmd -> (model, newCmd(model))
        RemoveRow f ->(f(model), Cmd.none)
        ToggleEdit tab -> (
            {model | tabs = Utils.updateOnWay model.tabs tab (\t -> {t | isEditing = not tab.isEditing})},
            Cmd.none
        )
        PersonalTabClicked -> ({ model | activeTab = ProfileTab }, Cmd.none)
        ClassesTabClicked -> ({ model | activeTab = ClassesTab }, Cmd.none)
        FavorsTabClicked -> ({ model | activeTab = FavorsTab }, Cmd.none)
        OtherTabClicked tab -> ({ model | activeTab = OtherTab tab }, Cmd.none)
        FormUpdated f ->(f(model), Cmd.none)
