{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

tokens :-
    DECLARE                     {return TDeclare}
    BEGIN                       {return TBegin}
    END                         {return TEnd}
    IF                          {return TIf}
    THEN                        {return TThen}
    ELSE                        {return TElse}
    ENDIF                       {return TEndif}
    WHILE                       {return TWhile}
    DO                          {return TDo}
    ENDWHILE                    {return TEndwhile}
    REPEAT                      {return TRepeat}
    UNTIL                       {return TUntil}
    FOR                         {return TFor}
    FROM                        {return TFrom}
    TO                          {return TTo}
    DOWNTO                      {return TDownto}
    ENDFOR                      {return TEndfor}
    READ                        {return TRead}
    WRITE                       {return TWrite}
    \(                          {return TOB}
    \)                          {return TCB}
    \;                          {return TS}
    \,                          {return TC}
    \:                          {return TCollon}
    \:\=                        {return TAssign}
    \+                          {return TPlus}
    \-                          {return TMinus}
    \*                          {return TMul}
    \/                          {return TDiv}
    \%                          {return TMod}
    \=                          {return TEq}
    \!\=                        {return TDiff}
    \<                          {return TLess}
    \>                          {return TMore}
    \<\=                        {return TLessEq}
    \>\=                        {return TMoreEq}
    [_a-z]+                     { \s -> TId s}
    [1-9][0-9]*|0               { \s -> TNum (read s)}
    \[[^\]\[]*\n?\]             ;
    [\ \t\n]                    ;
{    
lexer :: String -> [Token]
lexer = alexScanTokens

data Token
    = TDeclare
    | TBegin
    | TEnd
    | TIf
    | TThen
    | TElse
    | TEndif
    | TWhile
    | TDo
    | TEndwhile
    | TRepeat
    | TUntil
    | TFor
    | TFrom
    | TTo
    | TDownto
    | TEndfor
    | TRead
    | TWrite
    | TOB
    | TCB
    | TS
    | TCollon
    | TC
    | TAssign
    | TPlus
    | TMinus
    | TMul
    | TDiv
    | TMod
    | TEq
    | TDiff
    | TLess
    | TMore
    | TLessEq
    | TMoreEq
    | TId String
    | TNum Integer
    deriving (Eq,Show)
}