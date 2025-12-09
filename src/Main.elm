module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id)

import Scanner exposing (scan)
import Html.Events exposing (onClick)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = String

init : Model
init = """A
FORK L1
B
QUIT
L1: C"""

type Msg
  = ScanText
  | TextInput String

update : Msg -> Model -> Model
update msg model =
  case msg of
    TextInput input -> input
    ScanText ->
        case scan model of
            Ok tokens -> 
                let
                    _ = Debug.log "Tokens" tokens
                in
                    model
            Err errors -> 
                let
                    _ = Debug.log "Errors" errors
                in
                    model

view : Model -> Html Msg
view model =
    div []
            [ h1 [] [ text "Hello World!" ]
            , button [ onClick ScanText ] [ text "Press me" ]
            , textarea [ id "editor", value model, onInput TextInput] []
            ]