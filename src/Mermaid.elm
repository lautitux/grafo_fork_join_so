module Mermaid exposing (..)

import Compiler exposing (Graph)
import Dict
import Set


graphToMermaidJS : Graph -> String
graphToMermaidJS graph =
    let
        format k v acc =
            if String.isEmpty k && not (String.isEmpty v) then
                acc ++ "\t" ++ v ++ "\n"

            else if String.isEmpty v then
                acc ++ "\t" ++ k ++ "\n"

            else
                acc ++ "\t" ++ k ++ " --> " ++ v ++ "\n"
    in
    Dict.foldl
        format
        "graph TD\n"
        (Dict.map (\_ v -> String.join " & " (Set.toList v)) graph)
