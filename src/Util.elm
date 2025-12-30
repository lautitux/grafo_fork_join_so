module Util exposing (..)

dropWhile : (a -> Bool) -> List a -> (List a, Int)
dropWhile p list =
    let
        loop ls i =
            case ls of
                [] -> ([], i)
                x :: xs ->
                    if p x then
                        loop xs (i + 1)
                    else
                        (ls, i)
    in
        loop list 0
