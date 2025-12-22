port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Parse exposing (parse)
import Platform.Cmd as Cmd
import Compiler exposing (compile)

main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model = String


init : () -> ( Model, Cmd Msg )
init _ = ( "", Cmd.none )


type Msg 
    = Run
    | Send
    | Recv String

port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model = 
    case msg of
        Run ->
            let
                stmts = Result.withDefault [] (Debug.log "Parsed" (parse model))
                _ = Debug.log "Compiled" (compile stmts)
            in
                ( model, Cmd.none )
        Send -> ( model, Cmd.none )
        Recv code -> ( Debug.log "Model" code, Cmd.none )

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
                    , button [ onClick Run ] [ text "Run" ]
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