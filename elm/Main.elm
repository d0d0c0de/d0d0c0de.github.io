
import Html exposing (Html, Attribute, text, div, input, span)
import Html.Keyed as HK
import Html.Attributes exposing (value, style, class)
import Html.Events exposing (on, keyCode, onInput)
import Json.Decode as Json

main: Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- Custom event handler
onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

type alias Model = {
        ttyDisplay: TtyDisplay
    }

type alias TtyDisplay = {
        width: Int,
        height: Int,
        prompt: String,
        command: String,
        lastLineId: Int,
        lines: List (String, String)
    }
    
model: Model
model = Model (TtyDisplay 80 5 "prompt>" "" 0 [])

type Msg = MsgKeyDown Int
         | MsgInput String

update : Msg -> Model -> Model
update msg model =
    case msg of
        MsgKeyDown key ->
            if key == 13 then
                let ttyDisplay = model.ttyDisplay
                in {model | ttyDisplay = {ttyDisplay |
                                              lines = List.take (ttyDisplay.height - 1) ((toString ttyDisplay.lastLineId, ttyDisplay.command) :: ttyDisplay.lines),
                                              command = "",
                                              lastLineId = ttyDisplay.lastLineId + 1}}
            else
                model
        MsgInput txt ->
            let ttyDisplay = model.ttyDisplay
            in {model | ttyDisplay = {ttyDisplay | command = txt}}

view: Model -> Html Msg
view model =
    viewTtyDisplay model.ttyDisplay

viewTtyDisplay: TtyDisplay -> Html Msg
viewTtyDisplay ttyDisplay =
    HK.ul [] (viewTtyDisplayContent False ttyDisplay.height (List.reverse ttyDisplay.lines) ttyDisplay)

viewTtyDisplayContent: Bool -> Int -> List (String, String) -> TtyDisplay -> List ((String, Html Msg))
viewTtyDisplayContent inputDone height lines ttyDisplay =
    case (inputDone, height, lines) of
        (_, 0, _) ->
            []
        (False, h, []) -> 
            ("prompt", (div [] [span [class "ttyPrompt"] [text ttyDisplay.prompt], input [value ttyDisplay.command, onKeyDown MsgKeyDown, onInput MsgInput] []]))
            :: (viewTtyDisplayContent True (h - 1) [] ttyDisplay)
        (i, h, ((k,v)::xs)) ->
            (k, (div [class "ttyLine"] [text v]))
            :: (viewTtyDisplayContent i (h - 1) xs ttyDisplay)
        (i, h, []) ->
            ("null", (div [class "ttyLine"] [text ""]))
            :: (viewTtyDisplayContent i (h - 1) [] ttyDisplay)
        
