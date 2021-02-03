module Assigments(removeUnnecessaryAssignments) where

import Debug
import Debug.Trace

import Parser
    ( Command(..),
      Condition(..),
      Expression(..),
      Identifier(..),
      Value(..) )

import Data.Set ( Set, member, insert, delete, empty, union, fromList )

type Used = Set Identifier

-- Checks if identifier was used
isUsed :: Used -> Identifier -> Bool
isUsed used id@(SingleVar _) = member id used
isUsed used id@(ArrayConst name _) = member id used || any (dynamicWithName name) used
    where
        dynamicWithName n' (ArrayVar n _)  = n == n'
        dynamicWithName _ _ = False
isUsed used id@(ArrayVar name _) = any (arrayWithName name) used
    where
        arrayWithName n' (ArrayVar n _)  = n == n'
        arrayWithName n' (ArrayConst n _)  = n == n'
        arrayWithName _ _ = False

-- Markes or Unmarkes identifier as used
updateUsed :: Used -> Identifier -> Bool -> Used
updateUsed used id state
    | state = insert id used 
    | otherwise = delete id used


-- Markes as used whole list of identifiers
usedListOfId :: Used -> [Identifier] -> Used
usedListOfId= Prelude.foldl (\r id -> updateUsed r id True)

-- Returns list of Identifiers used in specific value
usedInValue :: Value -> [Identifier]
usedInValue (Const _) = []
usedInValue (Var v@(SingleVar _)) = [v]
usedInValue (Var v@(ArrayConst _ _)) = [v]
usedInValue (Var v@(ArrayVar _ str)) = [v, SingleVar str]

-- Returns list of Identifiers used in specific expresion
usedInExpresson :: Expression -> [Identifier]
usedInExpresson exp = case exp of
    SingleValue v -> usedInValue v
    Add v1 v2 -> usedInValue v1 ++ usedInValue v2 
    Sub v1 v2 -> usedInValue v1 ++ usedInValue v2 
    Mul v1 v2 -> usedInValue v1 ++ usedInValue v2 
    Div v1 v2 -> usedInValue v1 ++ usedInValue v2 
    Mod v1 v2 -> usedInValue v1 ++ usedInValue v2 

usedInCondition :: Condition -> [Identifier]
usedInCondition (Cond v1 _ v2) = usedInValue v1 ++ usedInValue v2 

-- Using set of used variables removing unnecessary assigments and returns new set of used variables
assigmentsFold :: Used -> [Command] -> (Used, [Command])
assigmentsFold used list = go [] used (reverse list)
    where
        go :: [Command] -> Used -> [Command] -> (Used, [Command])
        go acc used [] = (used, acc)
        go acc used (Assign id exp:rest)
            | exp == SingleValue (Var id) = go acc used rest
            | isUsed used id = go (Assign id exp:acc) used'' rest
            | otherwise = go acc used rest
            where                
                used' = case id of
                    ArrayVar name index -> updateUsed (updateUsed used (SingleVar index) True) id False
                    _ -> updateUsed used id False
                idsInExp = usedInExpresson exp
                used'' = usedListOfId used' idsInExp
        go acc used (IfElse cond ins ins':rest) = if null ifFolded && null elseFolded
            then go acc used rest
            else go (IfElse cond ifFolded elseFolded:acc) used' rest
            where
                (ifUsed, ifFolded) =  assigmentsFold used ins
                (elseUsed, elseFolded) = assigmentsFold used ins'
                condUsed = usedInCondition cond
                used' = ifUsed `union` elseUsed `union` fromList condUsed
        go acc used (If cond ins :rest) = if null ifFolded
            then go acc used rest
            else go (If cond ifFolded:acc) used' rest
            where
                (ifUsed, ifFolded) = assigmentsFold used ins
                condUsed = usedInCondition cond
                used' = ifUsed `union` used `union` fromList condUsed
        go acc used (While cond ins :rest) = if null loopFolded 
            then go acc used rest
            else go (While cond loopFolded:acc) used' rest
            where
                (afetrLoop, _) = assigmentsFold used ins
                condUsed = usedInCondition cond
                used' = afetrLoop `union` used `union` fromList condUsed
                (_, loopFolded) = assigmentsFold used' ins
        go acc used (Until cond ins :rest) = if null loopFolded 
            then go acc used rest
            else go (Until cond loopFolded:acc) used' rest
            where
                (afetrLoop, _) = assigmentsFold used ins
                condUsed = usedInCondition cond
                used' = afetrLoop `union` used `union` fromList condUsed
                (_, loopFolded) = assigmentsFold used' ins
        go acc used (ForUp str v1 v2 ins :rest) = if null loopFolded 
            then go acc used rest
            else go (ForUp str v1 v2 loopFolded:acc) used'' rest
            where
                (afetrLoop, _) = assigmentsFold used ins
                used' = afetrLoop `union` used
                loopBoundUsed = usedInValue v1 ++ usedInValue v2
                (_, loopFolded) = assigmentsFold used' ins
                used'' = usedListOfId used' loopBoundUsed
        go acc used (ForDown str v1 v2 ins :rest) = if null loopFolded 
            then go acc used rest
            else go (ForDown str v1 v2 loopFolded:acc) used'' rest
            where
                (afetrLoop, _) = assigmentsFold used ins
                used' = afetrLoop `union` used
                loopBoundUsed = usedInValue v1 ++ usedInValue v2
                (_, loopFolded) = assigmentsFold used' ins
                used'' = usedListOfId used' loopBoundUsed
        go acc used (Read id:rest) = go (Read id:acc) used' rest
            where
                used' = case id of
                    ArrayVar name index -> updateUsed (updateUsed used (SingleVar index) True) id False
                    _ -> updateUsed used id False
        go acc used (Write v:rest) = go (Write v:acc) used' rest
            where
                used' = usedListOfId used (usedInValue v) 

-- Function removing assigment which are never used       
removeUnnecessaryAssignments :: [Command] -> [Command]
removeUnnecessaryAssignments commands = res
    where
        (_, res) = assigmentsFold empty commands
        