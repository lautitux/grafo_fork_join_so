module Util exposing (..)

dropWhile : (a -> Bool) -> List a -> List a
dropWhile p ls =
    case ls of
        [] -> []
        x :: xs ->
            if p x then
                dropWhile p xs
            else
                ls

takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
    let
        loop ls acc =
            case ls of
                [] -> List.reverse acc
                x :: xs ->
                    if p x then
                        loop xs (x :: acc)
                    else
                        List.reverse acc
    in
        loop list []
