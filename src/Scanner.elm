module Scanner exposing (Token, TokenType, scan)

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


keyword : String -> Maybe TokenType
keyword k =
    case String.toUpper k of
        "FORK" ->
            Just Fork

        "GOTO" ->
            Just Goto

        "JOIN" ->
            Just Join

        "QUIT" ->
            Just Quit

        _ ->
            Nothing

type alias ScanError =
    { msg : String, line : Int }

type alias State =
    { source : String
    , tokens : List Token
    , errors : List ScanError
    , line : Int
    }

incLine : State -> State
incLine s = { s | line = s.line + 1 }

addToken : State -> TokenType -> State
addToken s tt =
    { s | tokens = { ttype = tt, line = s.line } :: s.tokens }

addError : State -> String -> State
addError s err = { s | errors = { msg = err, line = s.line } :: s.errors }

skipLine : State -> State
skipLine s =
    case String.uncons s.source of
        Nothing -> s
        Just (c, cs) ->
            if c == '\n' then
                incLine s
            else
                skipLine { s | source = cs }

scanIdentStr : State -> String -> State
scanIdentStr s acc =
    case String.uncons s.source of
        Nothing -> addError s "Unterminated string literal"
        Just (c, cs) ->
            if c == '\'' then
                addToken { s | source = cs } (Identifier acc)
            else if c == '\n' then
                incLine (addError { s | source = cs } "Unterminated string literal")
            else
                scanIdentStr { s | source = cs } (acc ++ String.fromChar c)

scanIdent : State -> String -> State
scanIdent s acc =
    case String.uncons s.source of
        Nothing -> addToken s (Identifier acc)
        Just (c, cs) ->
            if Char.isAlphaNum c || c == '_' || c == '-' then
                scanIdent { s | source = cs } (acc ++ String.fromChar c)
            else
                case keyword acc of
                    Nothing -> addToken s (Identifier acc)
                    Just k -> addToken s k

toInt : String -> Int
toInt str = Maybe.withDefault 0 (String.toInt str)

scanInt : State -> String -> State
scanInt s acc =
    case String.uncons s.source of
        Nothing -> addToken s (Integer (toInt acc))
        Just (c, cs) ->
            if Char.isDigit c then
                scanIdent { s | source = cs } (acc ++ String.fromChar c)
            else
                addToken s (Integer (toInt acc))

scan2 : State -> State
scan2 s =
    case String.uncons s.source of
        Nothing ->
            addToken s EOF
        Just ( c, cs ) ->
            let
                ns = { s | source = cs }
            in
                scan2 
                    (case c of
                        '='  -> addToken ns Equal
                        ':'  -> addToken ns Colon
                        ','  -> addToken ns Comma
                        ';'  -> skipLine ns       -- Comments
                        ' '  -> ns                -- Whitespace
                        '\t' -> ns                -- Whitespace
                        '\r' -> ns                -- Whitespace
                        '\n' -> incLine ns
                        '\'' -> scanIdentStr ns ""
                        _ ->
                            if Char.isAlpha c then
                                scanIdent ns (String.fromChar c)
                            else if Char.isDigit c then
                                scanInt ns (String.fromChar c)
                            else
                                addError ns ("Unexpected character '" ++ String.fromChar c ++ "'")
                    )

scan : String -> Result (List ScanError) (List Token)
scan source =
    let
        result = scan2 { source = source, tokens = [], errors = [], line = 1 }
    in
        if List.isEmpty result.errors then
            Result.Ok (List.reverse result.tokens)
        else
            Result.Err (List.reverse result.errors)
        