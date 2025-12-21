module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Compiler exposing (compile)
import Parse exposing (parse)

main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model = ()


init : Model
init = ()


type Msg = Click

program =
    """ContF=2\n
ContG=3\n
FORK LA\n
FORK LD\n
H\n
FORK LJF\n
GOTO LJG\n
LD: D\n
FORK LE\n
GOTO LJF\n
QUIT\n
LE: E\n
GOTO LJG\n
LA: A\n
FORK LC\n
B\n
QUIT\n
LC: C\n
LJG: JOIN ContG, LG\n
QUIT\n
LG: G\n
QUIT\n
LJF: JOIN ContF, LF\n
QUIT\n
LF: F"""

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Click ->
            let
                stmts = Result.withDefault [] (Debug.log "Parsed" (parse program))
                graph = compile stmts
                _ = Debug.log "Graph" graph
            in
                ()

view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "top-bar" ] 
            [ h1 [] [ text "Fork & Join" ]
            , div [ id "controls" ]
                [ span []
                    [ select [ style "margin-right" "1em" ] 
                        [ option [] [ text "Examples" ]
                        ]
                    , button [ onClick Click ] [ text "Run" ]
                    ]
                , span []
                    [ text "Export as: "
                    , a [ href "#" ] [ text "PNG" ]
                    , text " | "
                    , a [ href "#" ] [ text "SVG" ]
                    ]
                ]
            ]
        , main_ [] 
            [ div [ id "editor" ][]
            , div [ id "graph" ] []
            ]
        , footer [] 
            [ p [ style "text-align" "center" ] 
                [ text "Created by "
                , a [ href "https://github.com/lautitux" ] 
                    [ text "Lautaro Montes" ]
                , text " in 2025"
                ]
            ]
        ]