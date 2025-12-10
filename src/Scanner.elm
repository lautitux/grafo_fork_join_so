module Scanner exposing (Token, TokenType(..), ScanError, scan)
import Util

type alias Token =
    { ttype : TokenType, line : Int }

type TokenType
    = Identifier String
    | Integer Int
    | Equal
    | Colon
    | Comma
    | Fork
    | Goto
    | Join
    | Quit
    | EOF

type alias ScanError =
    { msg : String, line : Int }

type alias State =
    { source : List Char
    , tokens : List Token
    , errors : List ScanError
    , line : Int
    }

keyword : String -> Maybe TokenType
keyword k =
    case String.toUpper k of
        "FORK" -> Just Fork
        "GOTO" -> Just Goto
        "JOIN" -> Just Join
        "QUIT" -> Just Quit
        _ -> Nothing

incLine : State -> State
incLine state = { state | line = state.line + 1 }

advance : State -> State
advance state = { state | source = List.drop 1 state.source }

addToken : State -> TokenType -> State
addToken state tt =
    { state | tokens = { ttype = tt, line = state.line } :: state.tokens }

addError : State -> String -> State
addError state err = { state | errors = { msg = err, line = state.line } :: state.errors }

dropWhile : (Char -> Bool) -> State -> State
dropWhile p state = { state | source = Util.dropWhile p state.source }

takeWhile : (Char -> Bool) -> State -> (String, State)
takeWhile p state =
    let
        str = String.fromList (Util.takeWhile p state.source)
    in
        (str, { state | source = List.drop (String.length str) state.source })

peek : State -> Maybe Char
peek state =
    case state.source of
        [] -> Nothing
        c :: _ -> Just c

scanTokens : State -> State
scanTokens state =
     case state.source of
        [] -> addToken state EOF
        '=' :: _  -> scanTokens (advance (addToken state Equal))
        ':' :: _  -> scanTokens (advance (addToken state Colon))
        ',' :: _  -> scanTokens (advance (addToken state Comma))
        ';' :: _  -> scanTokens (dropWhile (\c -> c /= '\n') state)
        '\'' :: _ -> scanTokens (stringIdent (advance state))
        ' ' :: _  -> scanTokens (advance state)
        '\t' :: _ -> scanTokens (advance state)
        '\r' :: _ -> scanTokens (advance state)
        '\n' :: _ -> scanTokens (incLine (advance state))
        c :: _    ->
            scanTokens (
                if Char.isDigit c then
                    integer state
                else if Char.isAlpha c || c == '_' then
                    identifier state 
                else
                    addError (advance state) ("Unexpected character '" ++ String.fromChar c ++ "'.")
            )

stringIdent : State -> State
stringIdent s =
    let
        (str, new_state) = takeWhile (\c -> c /= '\n' && c /= '\'') s
    in
        if peek new_state == Nothing || peek new_state == (Just '\n') then
            addError new_state "Unterminated string."
        else
            addToken (advance new_state) (Identifier str)

identifier : State -> State
identifier s =
    let
        (str, new_state) = takeWhile (\c -> Char.isAlphaNum c || c == '_') s
    in
        case keyword str of
            Nothing -> addToken new_state (Identifier str)
            Just kwd -> addToken new_state kwd

integer : State -> State
integer state =
    let
        (str, ns) = takeWhile Char.isDigit state
    in
        addToken ns (Integer (Maybe.withDefault 0 (String.toInt str)))

scan : String -> Result (List ScanError) (List Token)
scan src =
    let
        res = scanTokens { source = String.toList src, tokens = [], errors = [], line = 1 }
    in
        if List.isEmpty res.errors then
            Result.Ok (List.reverse res.tokens)
        else
            Result.Err (List.reverse res.errors)