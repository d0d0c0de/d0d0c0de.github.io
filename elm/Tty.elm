module Tty exposing (Tty, Msg
                    , new, update, render
                    , addLine, addLinesFromList
                    , setLastLine, setLinesFromList
                    , setCommand
                    , setPrompt
                    )


import Html exposing (Html, Attribute, text, div, input, span)
import Html.Keyed
import Html.Attributes exposing (value, style, class)
import Html.Events exposing (on, keyCode, onInput)
import Json.Decode as Json


type alias Tty =
    { width: Int
    , height: Int
    , prompt: String
    , command: String
    , lastLineKey: Int
    , lines: List (String, String)
    }

    
new: Int -> Int -> String -> Tty
new width height prompt =
    Tty width height prompt "" 0 []

        
type Msg
    = KeyDown Int
    | Input String

      
update: Msg -> Tty -> (Tty, Cmd Msg)
update msg tty =
    case msg of
        KeyDown key ->
            if key == 13 then
                (setCommand "" (addLine tty.command tty), Cmd.none)
            else
                (tty, Cmd.none)
        Input txt ->
            (setCommand txt tty, Cmd.none)


addLine: String -> Tty -> Tty
addLine line tty =
    { tty
        | lines = List.take (tty.height - 1) ((toString tty.lastLineKey, line) :: tty.lines)
        , lastLineKey = tty.lastLineKey + 1
    }


addLinesFromList: List String -> Tty -> Tty
addLinesFromList lines tty =
    case lines of
        [] ->
            tty
        (x::xs) ->
            let
                newTty = addLine x tty
            in
                addLinesFromList xs newTty


setLastLine: String -> Tty -> Tty
setLastLine txt tty =
    case tty.lines of
        ((k, _)::xs) ->
            { tty
                | lines = (k, txt) :: xs
            }
        [] ->
            tty
            

setLinesFromList: List String -> Tty -> Tty
setLinesFromList lines tty =
    addLinesFromList lines { tty | lines = [] }
                
                
setCommand: String -> Tty -> Tty
setCommand cmd tty =
    { tty
        | command = cmd
    }
    

setPrompt: String -> Tty -> Tty
setPrompt prompt tty =
    { tty
        | prompt = prompt
    }


-- Custom event handler
onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)


render: Tty -> Html Msg
render tty =
    Html.Keyed.node "div" [class "elmTtyContainer"] (renderContent False tty.height (List.reverse tty.lines) tty)

        
renderContent: Bool -> Int -> List (String, String) -> Tty -> List ((String, Html Msg))
renderContent inputDone height lines tty =
    case (inputDone, height, lines) of
        (_, 0, _) ->
            []
        (False, h, []) -> 
            ("prompt", (div [] [span [class "ttyPrompt"] [text tty.prompt], input [value tty.command, onKeyDown KeyDown, onInput Input] []]))
            :: (renderContent True (h - 1) [] tty)
        (i, h, ((k,v)::xs)) ->
            (k, (div [class "ttyLine"] [text v]))
            :: (renderContent i (h - 1) xs tty)
        (i, h, []) ->
            ("empty" ++ (toString h), (div [class "ttyLine"] [text ""]))
            :: (renderContent i (h - 1) [] tty)
        
    
subscriptions: Tty -> Sub Msg
subscriptions tty =
    Sub.none
