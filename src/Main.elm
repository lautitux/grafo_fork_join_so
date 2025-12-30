port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Parse exposing (parse)
import Platform.Cmd as Cmd
import Compiler exposing (compile)
import Json.Encode as E
import Dict
import Set
import Parse exposing (Located)

main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
    { code : String
    , svg : Maybe String
    , error : Maybe (Located String)
    }


init : () -> ( Model, Cmd Msg )
init _ = ( { code = "", svg = Nothing, error = Nothing } , Cmd.none )


type Msg 
    = Run
    | ExportAs String
    | Update String
    | LoadSvg String

port renderGraph : E.Value -> Cmd msg
port exportAs : String -> Cmd msg
port editorUpdate : (String -> msg) -> Sub msg
port loadSVG : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
    [ editorUpdate Update
    , loadSVG LoadSvg
    ]

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model = 
    case msg of
        Run ->
            let
                serialize graph = E.object <| List.map (\(a, b) -> (a , E.list E.string (Set.toList b))) (Dict.toList graph)
            in
                case parse model.code of  
                    Ok stmts -> 
                        case Result.map serialize (compile stmts) of
                            Ok graph -> ( { model | error = Nothing }, renderGraph graph )
                            Err err -> ( { model | error = Just err, svg = Nothing }, Cmd.none )
                    Err deadEnds -> ( model, Cmd.none )
        ExportAs format -> ( model, exportAs format )
        Update code -> ( Debug.log "Model" { model | code = code }, Cmd.none )
        LoadSvg svg -> ( { model | svg = Just svg }, Cmd.none )

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
                , span [ hidden (model.svg == Nothing) ]
                    [ text "Export as: "
                    , a [ href "#", onClick (ExportAs "svg") ] [ text "SVG" ]
                    ]
                ]
            ]
        , main_ [] 
            [ div [ id "editor" ][]
            , div [ id "graph" ] <|
                case model.svg of
                    Just svg -> [ img [ src svg, alt "Graph Image" ] [] ]
                    Nothing -> 
                        case model.error of
                            Just err ->
                                [ div [id "error", class "message"] 
                                    [ p [] 
                                        [ strong [] 
                                            [ text 
                                                ( "Error [" 
                                                 ++ String.fromInt (Tuple.first err.start) 
                                                 ++ ":" 
                                                 ++ String.fromInt (Tuple.second err.start) 
                                                 ++ "]"
                                                )
                                            ]
                                        , br [] []
                                        , text err.value
                                        ]
                                    ]
                                ]
                            Nothing -> []
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