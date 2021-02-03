{
module Parser(parse, Declaration(..), Command(..), Expression(..), Condition(..), Value(..), Identifier(..)) where
import Lexer
import Translator
}

%monad {Either String}
%name parse
%tokentype { Token }
%error {parserError}

%token
    NUM         {TNum $$}
    ID          {TId $$}
    "DECLARE"   {TDeclare}
    "BEGIN"     {TBegin}
    "END"       {TEnd}
    "IF"        {TIf}
    "THEN"      {TThen}
    "ELSE"      {TElse}
    "ENDIF"     {TEndif}
    "WHILE"     {TWhile}
    "DO"        {TDo}
    "ENDWHILE"  {TEndwhile}
    "REPEAT"    {TRepeat}
    "UNTIL"     {TUntil}
    "FOR"       {TFor}
    "FROM"      {TFrom}
    "TO"        {TTo}
    "DOWNTO"    {TDownto}
    "ENDFOR"    {TEndfor}
    "READ"      {TRead}
    "WRITE"     {TWrite}
    '('         {TOB}
    ')'         {TCB}
    ';'         {TS}
    ':'         {TCollon}
    ','         {TC}
    ":="        {TAssign}
    '+'         {TPlus}
    '-'         {TMinus}
    '*'         {TMul}
    '/'         {TDiv}
    "mod"       {TMod}
    '='         {TEq}
    "!="        {TDiff}
    '<'         {TLess}
    '>'         {TMore}
    "<="        {TLessEq}
    ">="        {TMoreEq}

%%
program : "DECLARE" declarations "BEGIN" commands "END"     {($2, $4)}
        | "BEGIN" commands "END"                            {([], $2)}

declarations : ID ',' declarations                          {SingleDeclaration $1 : $3}
             | ID '(' NUM ':' NUM ')' ',' declarations      {ArrayDeclaration $1 $3 $5 : $8}
             | ID                                           {[SingleDeclaration $1]}
             | ID '(' NUM ':' NUM ')'                       {[ArrayDeclaration $1 $3 $5]}

commands : command commands                                 {$1:$2}
         | command                                          {[$1]}

command : identifier ":=" expression ';'                                        {Assign $1 $3}
        | "IF" condition "THEN" commands "ELSE" commands "ENDIF"                {IfElse $2 $4 $6}
        | "IF" condition "THEN" commands "ENDIF"                                {IfElse $2 $4 []}
        | "WHILE" condition "DO" commands "ENDWHILE"                            {While $2 $4}
        | "REPEAT" commands "UNTIL" condition ';'                               {Until $4 $2}
        | "FOR" ID "FROM" value "TO" value "DO" commands "ENDFOR"               {ForUp $2 $4 $6 $8}
        | "FOR" ID "FROM" value "DOWNTO" value "DO" commands "ENDFOR"           {ForDown $2 $4 $6 $8}
        | "READ" identifier ';'                                                 {Read $2}
        | "WRITE" value ';'                                                     {Write $2}

expression : value              {SingleValue $1}
           | value '+' value    {Add $1 $3}
           | value '-' value    {Sub $1 $3}
           | value '*' value    {Mul $1 $3}
           | value '/' value    {Div $1 $3}
           | value "mod" value  {Mod $1 $3}

condition : value '=' value     {Cond $1 Translator.EQ $3}
          | value "!=" value    {Cond $1 Translator.NE $3}
          | value '<' value     {Cond $1 Translator.LT $3}
          | value '>' value     {Cond $1 Translator.GT $3}
          | value "<=" value    {Cond $1 Translator.LE $3}
          | value ">=" value    {Cond $1 Translator.GE $3}

value : NUM                     {Const $1}
      | identifier              {Var $1}

identifier : ID                 {SingleVar $1}
           | ID '(' NUM ')'     {ArrayConst $1 $3}
           | ID '(' ID ')'      {ArrayVar $1 $3}
{
parserError t = Left $ "Syntax error at "++show t

data Declaration = SingleDeclaration String
    | ArrayDeclaration String Integer Integer deriving (Show, Eq) 

data Command = Assign Identifier Expression
    | IfElse Condition [Command] [Command]
    | If Condition [Command]
    | While Condition [Command]
    | Until Condition [Command]
    | ForUp String Value Value [Command]
    | ForDown String Value Value [Command]
    | Read Identifier
    | Write Value
    deriving (Eq) 

data Expression = SingleValue Value
    | Add Value Value
    | Sub Value Value
    | Mul Value Value
    | Div Value Value
    | Mod Value Value
    deriving (Eq)

data Condition = Cond Value Comparator Value deriving (Eq) 

data Value = Const Integer
    | Var Identifier
    deriving (Eq, Ord)

data Identifier = SingleVar String
    | ArrayConst String Integer
    | ArrayVar String String deriving (Eq, Ord)
}

