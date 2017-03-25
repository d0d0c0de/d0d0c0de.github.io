
import Tty
import Html exposing (Html)


main: Program Never Model Msg
main =
  Html.program { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    {
        tty: Tty.Tty
    }


init: (Model, Cmd Msg)
init =
    (Model (Tty.new 80 10 "prompt>"), Cmd.none)


type Msg =
    TtyMsg Tty.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TtyMsg m ->
            let
                (tty, cmd) = Tty.update m model.tty
            in
                ({ model | tty = tty }, Cmd.map TtyMsg cmd)


view: Model -> Html Msg
view model =
    Html.map TtyMsg (Tty.render model.tty)


subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none
