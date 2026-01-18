port module Main exposing (main)

import Browser
import Compiler exposing (compile)
import Dict
import Examples exposing (examples, examplesDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Mermaid exposing (graphToMermaidJS)
import Parse exposing (Located, parse)
import Parser exposing (Problem(..))
import Platform.Cmd as Cmd
import Util exposing (deadEndToLocatedString)


main : Program () Model Msg
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
    , graph : Maybe String
    , error : Maybe (Located String)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = "", graph = Nothing, svg = Nothing, error = Nothing }, Cmd.none )


type Msg
    = Run
    | Export String
    | Update String
    | SvgImage String
    | LoadExample String


port renderGraph : String -> Cmd msg


port export : String -> Cmd msg


port editorLoadExample : String -> Cmd msg


port editorUpdate : (String -> msg) -> Sub msg


port svgImage : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ editorUpdate Update
        , svgImage SvgImage
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            case parse model.code of
                Ok stmts ->
                    case Result.map graphToMermaidJS (Debug.log "Compiled" <| compile stmts) of
                        Ok graph ->
                            ( { model | error = Nothing, graph = Just graph }, renderGraph graph )

                        Err err ->
                            ( { model | error = Just err, svg = Nothing, graph = Nothing }, Cmd.none )

                Err deadEnds ->
                    case deadEnds of
                        [] ->
                            ( model, Cmd.none )

                        deadEnd :: _ ->
                            ( { model | error = Just (deadEndToLocatedString deadEnd), svg = Nothing }, Cmd.none )

        Export graph ->
            ( model, export graph )

        Update code ->
            ( Debug.log "Model" { model | code = code }, Cmd.none )

        SvgImage svg ->
            ( { model | svg = Just svg }, Cmd.none )

        LoadExample key ->
            case Dict.get key examplesDict of
                Just example ->
                    ( model, editorLoadExample example )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "top-bar" ]
            [ h1 [] [ text "Fork & Join" ]
            , div [ id "controls" ]
                [ span []
                    [ select [ style "margin-right" "1em", onInput LoadExample ]
                        (option [ selected True, disabled True ] [ text "Examples" ]
                            :: List.map (\( name, _ ) -> option [ value name ] [ text name ]) examples
                        )
                    , button [ onClick Run ] [ text "Run" ]
                    ]
                , span [ hidden (model.graph == Nothing) ]
                    [ text "Export as: "
                    , a [ href "#", onClick (Export <| Maybe.withDefault "" model.graph) ] [ text "SVG" ]
                    ]
                ]
            ]
        , main_ []
            [ div [ id "editor" ] []
            , div [ id "graph" ] <|
                case model.svg of
                    Just svg ->
                        [ img [ id "graph-img", src svg, alt "Graph Image" ] [] ]

                    Nothing ->
                        case model.error of
                            Just err ->
                                [ div [ id "error", class "message" ]
                                    [ p []
                                        [ strong []
                                            [ text
                                                ("Error ["
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

                            Nothing ->
                                []
            ]
        , footer [ style "text-align" "center" ]
            [ h2 [] [ text "Quick language reference" ]
            , div [ style "display" "inline-block", style "margin" "0 auto" ]
                [ pre [ style "text-align" "start" ]
                    [ text """=========================================================
;          -> Comment
Name = X   -> Counter definition
FORK L     -> Start new thread at label L
GOTO L     -> Jump current thread to label L
JOIN C, L  -> Wait for C threads, then jump to L
QUIT       -> End current thread
========================================================="""
                    ]
                ]
            , p []
                [ text "Created by "
                , a [ href "https://github.com/lautitux" ]
                    [ text "Lautaro Montes" ]
                , text " in 2025"
                ]
            ]
        ]
