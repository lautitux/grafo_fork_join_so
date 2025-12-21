module Compiler exposing (..)

import Dict exposing (Dict)
import Parse exposing (Located, Statement(..))
import Set exposing (Set)
import Util
import Html.Attributes exposing (name)


type alias State =
    { graph : Dict String (List String)
    , counters : Dict String Int
    , labels : Set String
    , source : List (Located Statement)
    , current : List (Located Statement)
    , error : Maybe (Located String)
    }


located : Located Statement -> String -> Located String
located stmt str =
    Located stmt.start str stmt.end


advance : State -> State
advance state =
    { state | current = List.drop 1 state.current }

goto : State -> (Located Statement) -> String -> State
goto state stmt lbl =
    let
        rest = List.drop 1 (Util.takeWhile (\s -> s.value /= (Label lbl)) state.source)
    in
        case rest of
            [] -> { state | error = Just (located stmt ("Attempted a jump to a non existing label '" ++ lbl ++"'.")) }
            _ -> { state | current = rest }

merge : State -> State -> State
merge state1 state2 =
    { labels = Set.union state1.labels state2.labels
    , source = state1.source
    , current = state1.current
    , error = state2.error
    , counters = Dict.union state1.counters state2.counters
    , graph = Dict.merge
        (\vertex edges graph -> Dict.insert vertex edges graph)
        (\vertex edges1 edges2 graph -> Dict.insert vertex (Set.toList (Set.fromList <| edges1 ++ edges2)) graph)
        (\vertex edges graph -> Dict.insert vertex edges graph)
        state1.graph
        state2.graph
        Dict.empty
    }

compileStatement : State -> String -> State
compileStatement state parent =
    case state.current of
        [] -> state
        stmt :: _ ->
            case stmt.value of
                Counter counter val ->
                    case Dict.get counter state.counters of
                        Nothing ->
                            compileStatement (advance { state | counters = Dict.insert counter val state.counters }) parent
                        Just _ ->
                            ({state |  
                              error = Just (located stmt ("Attempted re-assign of an already existing counter '" ++ counter ++ "'."))
                            })
                Goto lbl ->
                    let
                        new_state = goto state stmt lbl
                    in
                        case new_state.error of
                            Nothing -> compileStatement (new_state) parent
                            Just _ -> new_state
                Fork lbl ->
                    compileStatement (merge (advance state) (compileStatement (goto state stmt lbl) parent)) parent
                Join counter lbl ->
                    case Dict.get counter state.counters of
                        Just val ->
                            if val == 0 then
                                let
                                    new_state = goto state stmt lbl
                                in
                                    case new_state.error of
                                        Nothing -> compileStatement (new_state) parent
                                        Just _ -> new_state
                            else
                                compileStatement (advance { state | counters = Dict.insert counter (val - 1) state.counters }) parent
                        Nothing ->
                            { state | error = Just (located stmt ("Counter '" ++ counter ++ "' does not exist.")) }
                Label _ -> compileStatement (advance state) parent
                Quit -> state
                Process name ->
                    compileStatement
                        (advance 
                            { state |
                            graph =
                                Dict.insert 
                                    name
                                    (case Dict.get parent state.graph of
                                        Nothing -> [name]
                                        Just children -> name :: children)
                                    state.graph
                            }
                        )
                        name

compile : List (Located Statement) -> Result (Located String) (Dict String (List String))
compile stmts = 
    let
        state = compileStatement { graph = Dict.empty, counters = Dict.empty, labels = Set.empty, source = stmts, current = stmts, error = Nothing } ""
    in
        case state.error of
            Nothing -> Ok state.graph
            Just err -> Err err