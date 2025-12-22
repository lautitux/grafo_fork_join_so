module Compiler exposing (..)

import Dict exposing (Dict)
import Parse exposing (Located, Statement(..))
import Util
import Html.Attributes exposing (value)
import Set exposing (Set)

type Step
    = Continue State
    | Merge State Step
    | End State
    | Error (Located String)

type Counter
    = Value (Located Int)
    | Poisoned (Located String)

type alias State =
    { source : List (Located Statement)
    , current : List (Located Statement)
    , graph : Dict String (Set String)
    , counters : Dict String Counter
    }

unreachable : String -> Located String
unreachable s = Located (0, 0) ("Unreachable " ++ s ++ ".") (0, 0)

locate : (Located a) -> b -> (Located b)
locate ref val = Located ref.start val ref.end

goto : State -> String -> Result String State
goto state lbl =
    case Util.dropWhile (\s -> s.value /= Label lbl) state.source of
        [] -> Err ("Failed to find label '" ++ lbl ++ "'.")
        stmts -> Ok { state | current = stmts }

merge : State -> State -> State
merge state1 state2 =
    let
        keep : comparable -> v -> Dict comparable v -> Dict comparable v
        keep comparable v dict = Dict.insert comparable v dict
    in
        { source   = state1.source
        , current  = state1.current
        , counters = Dict.union state2.counters state1.counters
        , graph =
            Dict.merge
                keep
                (\parent a b dict -> Dict.insert parent (Set.union a b) dict)
                keep
                state1.graph
                state2.graph
                Dict.empty
        }


compileStatements : String -> Step -> Step
compileStatements parent step =
    case step of
        Merge state1 merge_step -> 
            case merge_step of
                End state2 -> compileStatements parent (Continue <| merge state1 state2)
                Error _ -> merge_step
                _ -> 
                    let
                        _ = Debug.log "Merge Step" merge_step
                    in
                        Error <| unreachable "Merge"
        Continue state ->
            case state.current of
                [] -> End state
                stmt :: rest ->
                    compileStatements parent <|
                    case stmt.value of
                        Label _ -> Continue { state | current = rest }
                        Counter counter value ->
                            case Dict.get counter state.counters of
                                Nothing -> 
                                    Continue
                                        { state 
                                        | counters = Dict.insert counter (Value <| locate stmt value) state.counters
                                        , current = rest
                                        }
                                Just _ ->
                                    Error <| locate stmt ("Cannot re-assign to already assigned counter '" ++ counter ++ "'.")
                        Fork lbl ->
                            case goto state lbl of
                                Ok goto_state -> Merge { state | current = rest } <| compileStatements parent (Continue goto_state)
                                Err msg -> Error <| locate stmt msg
                        Goto lbl ->
                            case goto state lbl of
                                Ok goto_state -> Continue goto_state
                                Err msg -> Error <| locate stmt msg
                        Join counter lbl ->
                            case Dict.get counter state.counters of
                                Nothing -> Error <| locate stmt ("Cannot join on non-existing counter '" ++ counter ++ "'.")
                                Just (Value count) ->
                                    case goto state lbl of
                                        Ok goto_state -> 
                                            Continue 
                                                { goto_state 
                                                | counters = Dict.insert counter (Value <| locate count (count.value - 1)) state.counters
                                                }
                                        Err msg -> Error <| locate stmt msg
                                Just (Poisoned error) -> Error error
                        Quit -> End state
                        Process name ->
                            compileStatements name <|
                                Continue
                                    { state
                                    | graph = 
                                        case Dict.get parent state.graph of
                                            Nothing -> Dict.insert parent (Set.insert name Set.empty) state.graph
                                            Just other -> Dict.insert parent (Set.insert name other) state.graph
                                    , current = rest
                                    }
        otherwise -> otherwise
    

compile : List (Located Statement) -> Result (Located String) (Dict String (Set String))
compile stmts =
    case compileStatements "" (Continue { source = stmts, current = stmts, graph = Dict.empty, counters = Dict.empty }) of
        End state -> Ok state.graph
        Error err -> Err err
        _ -> Err <| unreachable "Compile"