module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Scanner exposing (Token, ScanError, scan)
import Html.Attributes exposing (style)
import Html.Attributes exposing (class)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { code : String
    , tokens : List Token
    , errors : List ScanError
    }


init : Model
init =
    { code = ""
    , tokens = []
    , errors = []
    }


type Msg
    = ScanTokens
    | TextInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
      TextInput input -> { model | code = input }
      ScanTokens -> 
        case scan model.code of
          Ok tokens -> { model | tokens = tokens, errors = [] }
          Err errors -> { model | errors = errors }

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello World!" ]
        , button [ onClick ScanTokens ] [ text "Press me" ]
        , textarea [ id "editor", value model.code, onInput TextInput ] []
        , div [] (
            if List.isEmpty model.errors then
                List.map tokenToHtml model.tokens
            else
                List.map errorToHtml model.errors
          )
        ]

tokenToHtml : Token -> Html Msg
tokenToHtml t =
    span [ class "tk" ] [
        (
            case t.ttype of
                Scanner.Colon -> text ":"
                Scanner.Comma -> text ","
                Scanner.EOF   -> strong [] [ text "EOF" ]
                Scanner.Equal -> text "="
                Scanner.Fork  -> strong [] [ text "FORK" ]
                Scanner.Goto  -> strong [] [ text "GOTO" ]
                Scanner.Join  -> strong [] [ text "JOIN" ]
                Scanner.Quit  -> strong [] [ text "QUIT" ]
                Scanner.Identifier str -> text str
                Scanner.Integer val -> text (String.fromInt val)
        )
    ]

errorToHtml : ScanError -> Html Msg
errorToHtml e =
    span [ class "error" ] 
         [ span [ class "line" ] [ text ("Error, line " ++ String.fromInt e.line ++ ":") ]
         , text e.msg
         ]