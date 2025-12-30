module Compiler exposing (..)

import Dict exposing (Dict)
import Parse exposing (Located, Statement(..))
import Util
import Set exposing (Set)
import Html.Attributes exposing (value)

type alias State =
    { source : List (Located Statement)
    , current : List (Located Statement)
    , i : Int
    , counters : Dict String Int
    , joins : Dict Int (Set String)
    , graph : Dict String (Set String)
    }

located : Located a -> b -> Located b
located loc val = Located loc.start val loc.end

advance : State -> State
advance state = { state | current = List.drop 1 state.current, i = state.i + 1 }

goto : State -> String -> Result String State
goto state lbl =
    case Util.dropWhile (\s -> s.value /= Label lbl) state.source of
        ([], _) -> Err ("Failed to find label '" ++ lbl ++ "'.")
        (stmts, i) -> Ok { state | current = stmts, i = i }

compileStatements : Set String -> Result (Located String) State -> Result (Located String) State
compileStatements parents result =
    case result of
        Err _ -> result
        Ok state ->
            case state.current of
                [] -> result
                stmt :: _ ->
                    case stmt.value of
                        Label _ -> compileStatements parents <| Ok (advance state)
                        Counter ident value ->
                            compileStatements parents <|
                            case Dict.get ident state.counters of
                                Nothing -> Ok (advance { state | counters = Dict.insert ident value state.counters })
                                Just _ -> Err <| located stmt ("Invalid re-assignment of already assigned counter '" ++ ident ++ "'.")
                        Fork lbl ->
                            compileStatements parents <|
                            case goto state lbl of
                                Ok goto_state ->
                                    let
                                        goto_result = compileStatements parents (Ok goto_state)
                                        new_state = advance state
                                    in
                                        Result.map (\s -> { s | current = new_state.current, i = new_state.i }) goto_result
                                Err msg -> Err  (located stmt msg)
                        Goto lbl ->
                            compileStatements parents (Result.mapError (located stmt) (goto state lbl))
                        Join ident lbl ->
                            case Dict.get ident state.counters of
                                Nothing -> Err <| located stmt ("Attempted join on non-existing counter '" ++ ident ++ "'.")
                                Just value ->
                                    let
                                        new_state = advance { state | counters = Dict.insert ident (value - 1) state.counters }
                                        join_parents =
                                            case Dict.get state.i state.joins of
                                                Nothing -> parents
                                                Just others -> Set.union parents others
                                    in
                                        if value == 1 then
                                            compileStatements join_parents (Result.mapError (located stmt) (goto new_state lbl))
                                        else
                                            compileStatements parents (Ok { new_state | joins = Dict.insert state.i join_parents state.joins })
                        Quit -> Ok (advance state)
                        Process ident ->
                            compileStatements (Set.fromList [ident]) <| Ok 
                                (advance
                                    { state
                                    | graph = Set.foldl 
                                        (\parent graph ->
                                            case Dict.get parent graph of
                                                Nothing -> Dict.insert parent (Set.fromList [ident]) graph
                                                Just children -> Dict.insert parent (Set.insert ident children) graph
                                        )
                                        state.graph
                                        parents
                                    }
                                )

compile : List (Located Statement) -> Result (Located String) (Dict String (Set String))
compile stmts =
    let
       state = 
            { source = stmts
            , current = stmts
            , i = 0
            , counters = Dict.empty
            , joins = Dict.empty
            , graph = Dict.empty
            } 
    in
        Result.map (\s -> s.graph) (compileStatements (Set.fromList [""]) (Ok state))
        