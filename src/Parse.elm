module Parse exposing (StatementKind(..), parse)
import Parser exposing (..)
import Set

-- statement ::= (<labeled_statement> | <unlabeled_statement>) <EOL>
-- labeled_statement ::= <label> | <label> <unlabeled_statement>
-- unlabeled_statement ::= <counter> | <application> | <process>
-- label ::= <IDENTIFIER> ":"
-- counter ::= <IDENTIFIER> "=" <INTEGER>
-- application ::= <nullary> | <unary> | <binary>
-- nullary ::= "QUIT"
-- unary ::= ("FORK" | "GOTO") " " <IDENTIFIER>
-- binary ::= "JOIN" " " <IDENTIFIER> "," <IDENTIFIER>
-- process ::= <IDENTIFIER>

-- IDENTIFIER ::= [a-zA-Z_][a-zA-Z0-9_]*
-- INTEGER ::= [0-9]+
-- EOL ::= "\n"

type StatementKind
    = Label String
    | Counter String Int
    | Fork String
    | Goto String
    | Join String String
    | Quit
    | Process String

type alias Located a =
  { start : (Int, Int)
  , value : a
  , end : (Int, Int)
  }

located : Parser a -> Parser (Located a)
located parser =
  succeed Located
    |= getPosition
    |= parser
    |= getPosition

spaces : Parser ()
spaces =
  chompWhile (\c -> c == ' ' || c == '\t' || c == '\r')

spacesOrNewLine : Parser ()
spacesOrNewLine =
  chompWhile (\c -> c == ' ' || c == '\t' || c == '\r' || c == '\n')

identifier : Parser String
identifier =
    variable 
        { start = \c -> Char.isAlpha c || c == '_'
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }

label : Parser StatementKind
label = 
    succeed Label
    |= identifier
    |. spaces
    |. symbol ":"

counter : Parser StatementKind
counter =
    succeed Counter
    |= backtrackable identifier
    |. backtrackable spaces
    |. symbol "="
    |. spaces
    |= int

unary : String -> (String -> StatementKind) -> Parser StatementKind
unary s map =
    succeed map
    |. keyword s
    |. spaces
    |= identifier

binary : String -> (String -> String -> StatementKind) -> Parser StatementKind
binary s map =
    succeed map
    |. keyword s
    |. spaces
    |= identifier
    |. spaces
    |. symbol ","
    |. spaces
    |= identifier

fork : Parser StatementKind
fork = unary "FORK" Fork

goto : Parser StatementKind
goto = unary "GOTO" Goto

join : Parser StatementKind
join = binary "JOIN" Join

quit : Parser StatementKind
quit = 
    succeed Quit
    |. keyword "QUIT"

application : Parser StatementKind
application =
    oneOf 
    [ quit
    , fork
    , goto
    , join
    ]

process : Parser StatementKind
process = 
    succeed Process
    |= identifier

unlabeled_statement : Parser StatementKind
unlabeled_statement =
    oneOf 
    [
        counter,
        application,
        process
    ]

labeled_statement : Parser (Located StatementKind, Maybe (Located StatementKind))
labeled_statement =
    succeed (\lbl stmt -> (lbl, stmt))
    |= backtrackable (located label)
    |= oneOf
        [ succeed Just
          |. backtrackable spaces
          |= located unlabeled_statement
        , succeed Nothing
        ]

statement : Parser (Located StatementKind, Maybe (Located StatementKind))
statement =
    succeed identity
    |= oneOf 
        [  labeled_statement
        , map (\stmt -> (stmt, Nothing)) (located unlabeled_statement)
        ]
    |. oneOf 
        [ symbol "\n"
        , end
        ]

statements : Parser (List (Located StatementKind))
statements =
  loop [] statementsHelp

statementsHelp : List (Located StatementKind) -> Parser (Step (List (Located StatementKind)) (List (Located StatementKind)))
statementsHelp stmts =
  oneOf
    [ succeed (
        \(lbl, maybe_stmt) -> 
            case maybe_stmt of
                Just stmt -> Loop (stmt :: lbl :: stmts)
                Nothing -> Loop (lbl :: stmts)
      )
        |= statement
    , succeed (Loop stmts)
        |. lineComment ";"
        |. spacesOrNewLine
    , succeed ()
        |. end
        |> map (\_ -> Done (List.reverse stmts))
    ]

parse : String -> Result (List DeadEnd) (List (Located StatementKind))
parse = run statements