module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model = ()


init : Model
init = ()


type alias Msg = ()


update : Msg -> Model -> Model
update msg model = ()

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
                    , button [] [ text "Run" ]
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