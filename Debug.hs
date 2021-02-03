{-# LANGUAGE FlexibleInstances #-}
module Debug where

import Parser
import Translator
import qualified Data.Map as Map

instance Show Expression where
    show (SingleValue v) = "<- "++show v
    show (Add v1 v2) = "<- "++show v1++" + "++show v2
    show (Sub v1 v2) = "<- "++show v1++" - "++show v2
    show (Mul v1 v2) = "<- "++show v1++" * "++show v2
    show (Div v1 v2) = "<- "++show v1++" / "++show v2
    show (Mod v1 v2) = "<- "++show v1++" % "++show v2

instance Show Comparator where
    show comp = case comp of
        Translator.LE -> "<="
        Translator.LT -> "<"
        Translator.EQ -> "=="
        Translator.NE -> "!="
        Translator.GT -> ">"
        Translator.GE -> ">="

instance Show Condition where
    show (Cond v1 op v2) = show v1 ++ " " ++ show op ++ " " ++ show v2

instance Show Value where
    show (Var id) = show id
    show (Const c) = show c

instance Show Identifier where
    show id = case id of
        SingleVar name -> name
        ArrayConst name int -> name++"["++show int++"]"
        ArrayVar name var -> name++"["++var++"]"

instance Show Command where
    show command = case command of
        Assign id exp -> show id++" "++show exp
        IfElse cond ins ins' -> "IFELSE "++show cond++" "++show ins++" "++show ins'
        If cond ins -> "IF "++show cond++" "++show ins
        While cond ins -> "WHILE "++show cond++" "++show ins
        Until cond ins -> "UNTIL "++show cond++" "++show ins
        ForUp str v1 v2 ins -> "FOR "++str++" FROM "++show v1++" TO "++show v2++" "++show ins
        ForDown str v1 v2 ins -> "FOR "++str++" FROM "++show v1++" DOWNTO "++show v2++" "++show ins
        Read id -> "READ "++show id
        Write v -> "WRITE "++show v

