module Parse exposing (Statement(..), Located, parse)
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

type Statement
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

label : Parser Statement
label = 
    succeed Label
    |= identifier
    |. spaces
    |. symbol ":"

counter : Parser Statement
counter =
    succeed Counter
    |= backtrackable identifier
    |. backtrackable spaces
    |. symbol "="
    |. spaces
    |= int

unary : String -> (String -> Statement) -> Parser Statement
unary s map =
    succeed map
    |. keyword s
    |. spaces
    |= identifier

binary : String -> (String -> String -> Statement) -> Parser Statement
binary s map =
    succeed map
    |. keyword s
    |. spaces
    |= identifier
    |. spaces
    |. symbol ","
    |. spaces
    |= identifier

fork : Parser Statement
fork = unary "FORK" Fork

goto : Parser Statement
goto = unary "GOTO" Goto

join : Parser Statement
join = binary "JOIN" Join

quit : Parser Statement
quit = 
    succeed Quit
    |. keyword "QUIT"

application : Parser Statement
application =
    oneOf 
    [ quit
    , fork
    , goto
    , join
    ]

process : Parser Statement
process = 
    succeed Process
    |= identifier

unlabeled_statement : Parser Statement
unlabeled_statement =
    oneOf 
    [
        counter,
        application,
        process
    ]

labeled_statement : Parser (Located Statement, Maybe (Located Statement))
labeled_statement =
    succeed (\lbl stmt -> (lbl, stmt))
    |= backtrackable (located label)
    |= oneOf
        [ succeed Just
          |. backtrackable spaces
          |= located unlabeled_statement
        , succeed Nothing
        ]

statement : Parser (Located Statement, Maybe (Located Statement))
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

statements : Parser (List (Located Statement))
statements =
  loop [] statementsHelp

statementsHelp : List (Located Statement) -> Parser (Step (List (Located Statement)) (List (Located Statement)))
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

parse : String -> Result (List DeadEnd) (List (Located Statement))
parse = run statements