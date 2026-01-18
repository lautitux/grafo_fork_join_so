module Util exposing (..)

import Parse exposing (Located)
import Parser exposing (DeadEnd, Problem(..))


dropWhile : (a -> Bool) -> List a -> ( List a, Int )
dropWhile p list =
    let
        loop ls i =
            case ls of
                [] ->
                    ( [], i )

                x :: xs ->
                    if p x then
                        loop xs (i + 1)

                    else
                        ( ls, i )
    in
    loop list 0


deadEndToLocatedString : DeadEnd -> Located String
deadEndToLocatedString deadEnd =
    { start = ( deadEnd.row, deadEnd.col )
    , value =
        case deadEnd.problem of
            Expecting str ->
                "Expected '" ++ str ++ "'."

            ExpectingInt ->
                "Expected an integer."

            ExpectingVariable ->
                "Expected an identifier."

            ExpectingKeyword kwd ->
                "Expected keyword '" ++ kwd ++ "'."

            ExpectingEnd ->
                "Expected end of script."

            ExpectingSymbol sym ->
                "Expected '" ++ String.replace "\n" "\\n" sym ++ "'."

            UnexpectedChar ->
                "Unexpected character."

            Problem problem ->
                problem

            _ ->
                "[UNREACHABLE]"
    , end = ( deadEnd.row, deadEnd.col )
    }
