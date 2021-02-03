{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

tokens :-
    koszt\:                     {return COST}
    \?                          {return INPUT}
    \>                          {return OUTPUT}
    [1-9][0-9]*|0               {\s -> NUM (read s)}
    \[                           {return COMMENT}
    \]                          {return END_COMMENT}
    testowe                          {return DATA_BEGIN}
    OUTPUT                      {return DATA_BEGIN}
    \.                          {return DOT}
    .                           ;
    [\n\t\r]                    ;
    
{
lexer :: String -> [Token]
lexer = alexScanTokens

data Token
    = COST
    | INPUT
    | OUTPUT
    | NUM Integer
    | DOT
    | COMMENT
    | END_COMMENT
    | DATA_BEGIN
    deriving(Eq, Show)
}