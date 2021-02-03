module BigConstSubstitution(addConstToMemory) where

import Parser
    ( Command(..),
      Condition(..),
      Declaration(SingleDeclaration),
      Expression(..),
      Identifier(SingleVar),
      Value(..) )
import qualified Data.List as List
import qualified Data.Map as Map

addConstToMemory :: [Command] -> ([Declaration], [Command])
addConstToMemory commands = (dec, assin++code)
    where
        (const, code) = substitute Map.empty commands
        assin = (\(c, n) -> Assign (SingleVar n) (SingleValue (Const c))) <$> Map.toList const
        dec = (\(_, n) -> SingleDeclaration n) <$> Map.toList const

optimizeVal :: Map.Map Integer String -> Value -> (Map.Map Integer String, Value)
optimizeVal prev (Var a) = (prev, Var a)
optimizeVal prev (Const c) = if c > 268435456
    then (Map.insert c ("-c"++show c) prev, Var $ SingleVar ("-c"++show c))
    else (prev, Const c)

substituteExp :: Map.Map Integer String -> Expression -> (Map.Map Integer String, Expression)
substituteExp prev exp = case exp of
    SingleValue v -> (prev', SingleValue v')
        where
            (prev', v') = optimizeVal prev v
    Add a b -> (prev', Add a' b')
        where
            (prev', a', b') = double prev a b
    Sub a b -> (prev', Sub a' b')
        where
            (prev', a', b') = double prev a b
    Mul a b -> (prev', Mul a' b')
        where
            (prev', a', b') = double prev a b
    Div a b -> (prev', Div a' b')
        where
            (prev', a', b') = double prev a b
    Mod a b -> (prev', Mod a' b')
        where
            (prev', a', b') = double prev a b
        

substituteCond :: Map.Map Integer String -> Condition -> (Map.Map Integer String, Condition)
substituteCond prev (Cond a op b) = (prev', Cond a' op b')
    where
        (prev', a', b') = double prev a b

double :: Map.Map Integer String -> Value -> Value -> (Map.Map Integer String, Value, Value)
double prev a b = (prev', a', b')
    where
        (p', a') = optimizeVal prev a
        (prev', b') = optimizeVal p' b

substitute :: Map.Map Integer String -> [Command] -> (Map.Map Integer String, [Command])
substitute prev [] = (prev, [])
substitute prev (x:xs) = case x of
    Assign id exp -> (const, Assign id exp':code)
        where
            (const, code) = substitute prev' xs 
            (prev', exp') = substituteExp prev exp
    IfElse cond ins ins2 -> (const, IfElse cond' ins' ins2':code)
        where
            (const, code) = substitute prev' xs 
            (p', cond') = substituteCond prev cond
            (p'', ins') = substitute prev ins
            (p''', ins2') = substitute prev ins2
            prev' = p' `Map.union` p'' `Map.union` p'''
    If cond ins -> (const, If cond' ins':code)
        where
            (const, code) = substitute prev' xs 
            (p', cond') = substituteCond prev cond
            (p'', ins') = substitute prev ins
            prev' = p' `Map.union` p''
    While cond ins -> (const, While cond' ins':code)
        where
            (const, code) = substitute prev' xs 
            (p', cond') = substituteCond prev cond
            (p'', ins') = substitute prev ins
            prev' = p' `Map.union` p''
    Until cond ins -> (const, Until cond' ins':code)
        where
            (const, code) = substitute prev' xs 
            (p', cond') = substituteCond prev cond
            (p'', ins') = substitute prev ins
            prev' = p' `Map.union` p''
    ForUp str v1 v2 ins -> (const, ForUp str v1' v2' ins':code)
        where
            (const, code) = substitute prev' xs 
            (p', v1', v2') = double prev v1 v2
            (p'', ins') = substitute prev ins
            prev' = p' `Map.union` p''
    ForDown str v1 v2 ins -> (const, ForDown str v1' v2' ins':code)
        where
            (const, code) = substitute prev' xs 
            (p', v1', v2') = double prev v1 v2
            (p'', ins') = substitute prev ins
            prev' = p' `Map.union` p''
    Read id -> (const, Read id:code)
        where
            (const, code) = substitute prev xs
    Write (Var v) -> (const, Write (Var v):code)
        where
            (const, code) = substitute prev xs
    Write (Const c) -> (Map.insert c ("-c"++show c) const, Write (Var $ SingleVar ("-c"++show c)):code)
        where
            (const, code) = substitute prev xs
