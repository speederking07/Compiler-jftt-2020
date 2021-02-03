module ConstantsFolding(foldConstants) where

import Parser
    (Command(..),
      Condition(..),
      Expression(..),
      Identifier(..),
      Value(..) )
import Prelude hiding (lookup)
import Data.Map (mapWithKey, toList, intersectionWith, empty, insert, lookup, Map )
import Translator ( Comparator(..) )
import qualified Data.Maybe
import Debug.Trace
import Debug

data Information = Constant Integer | Undefined | Input deriving(Eq, Show)

type Knowlage = Map Value Information

combineInformation :: Information -> Information -> Information
combineInformation Undefined _ = Undefined
combineInformation _ Undefined = Undefined
combineInformation Input _ = Input
combineInformation _ Input = Input
combineInformation (Constant c) (Constant c')
    | c == c' = Constant c
    | otherwise = Input

isConst :: Information -> Bool
isConst (Constant _) = True
isConst _ = False

noInfo :: Information -> Bool
noInfo a = not (isConst a)

getInfo :: Knowlage -> Value -> Information
getInfo _ (Const c) = Constant c
getInfo map v@(Var (SingleVar _)) = Data.Maybe.fromMaybe Undefined (lookup v map)
getInfo map v@(Var (ArrayConst _ _)) = Data.Maybe.fromMaybe Input (lookup v map)
getInfo map v@(Var (ArrayVar name var))
    | isConst varInfo = getInfo map (Var (ArrayConst name c))
    | otherwise = Input 
    where
        varInfo = getInfo map (Var (SingleVar var)) 
        Constant c = varInfo

-- Update knowlage 
updateKnowlage :: Knowlage -> Identifier -> Information -> Knowlage
updateKnowlage prev id@(SingleVar _) info = insert (Var id) info prev
updateKnowlage prev id@(ArrayConst name _) info = insert (Var id) info prev
updateKnowlage prev id@(ArrayVar name var) info 
    | isConst varInfo = insert (Var (ArrayConst name c)) info prev 
    | otherwise = insert (Var id) Input $ mapWithKey (markDepending name) prev
    where
        varInfo = getInfo prev (Var (SingleVar var)) 
        Constant c = varInfo
        markDepending n (Var(ArrayConst n' _)) info
            | n == n' = Input
            | otherwise = info
        markDepending _ _ info = info

-- Get combine knowlage from two diffrent points
combineKnowlage :: Knowlage -> Knowlage -> Knowlage
combineKnowlage = intersectionWith combineInformation

-- Get information about result of expresson
infoAboutExpression :: Knowlage -> Expression -> Information
infoAboutExpression knowlage (SingleValue v) = getInfo knowlage v
infoAboutExpression knowlage (Add v1 v2)
    | k1 == Undefined || k2 == Undefined = Undefined
    | k1 == Input || k2 == Input = Input
    | otherwise = Constant (c1+c2)
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
infoAboutExpression knowlage (Sub v1 v2)
    | v1 == v2 = Constant 0
    | k1 == Undefined || k2 == Undefined = Undefined
    | k1 == Input || k2 == Input = Input
    | c2 >= c1 = Constant 0
    | otherwise = Constant (c1-c2)
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
infoAboutExpression knowlage (Mul v1 v2)
    | k1 == Undefined || k2 == Undefined = Undefined
    | k1 == Input || k2 == Input = Input
    | otherwise = Constant (c1*c2)
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
infoAboutExpression knowlage (Div v1 v2)
    | k1 == Undefined || k2 == Undefined = Undefined
    | k1 == Input || k2 == Input = Input
    | c2 == 0 = Constant 0
    | otherwise = Constant (c1 `div` c2)
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
infoAboutExpression knowlage (Mod v1 v2)
    | v1 == v2 = Constant 0
    | k1 == Undefined || k2 == Undefined = Undefined
    | k1 == Input || k2 == Input = Input
    | c2 == 0 = Constant 0
    | otherwise = Constant (c1 `mod` c2)
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2

-- Returns simpler identifier
simplyfyId :: Knowlage -> Identifier -> Identifier 
simplyfyId knowlage (ArrayVar name id) = if isConst infoId then ArrayConst name c' else ArrayVar name id
            where
                infoId = getInfo knowlage (Var(SingleVar id))
                Constant c' = infoId
simplyfyId _ id = id

-- Returns simpler version on expresson
simplyfyValue :: Knowlage -> Value -> Value
simplyfyValue knowlage v
    | isConst infoV = Const c
    | otherwise = case v of
        Var(ArrayVar name id) -> if isConst infoId then Var(ArrayConst name c') else v
            where
                infoId = getInfo knowlage (Var(SingleVar id))
                Constant c' = infoId
        _ -> v
    where
        infoV = getInfo knowlage v
        Constant c = infoV

-- Returns simplyfied expresson
simplyfyExpersson :: Knowlage -> Expression -> Expression
simplyfyExpersson knowlage (SingleValue v) 
    | k == Undefined =  SingleValue (simplyfyValue knowlage v)
    | k == Input = SingleValue (simplyfyValue knowlage v)
    | otherwise = SingleValue (Const c)
        where
            k = getInfo knowlage v
            Constant c = k
simplyfyExpersson knowlage (Add v1 v2)
    | noInfo k1 && noInfo k2 && v1 == v2 = Mul v1 (Const 2) 
    | noInfo k1 && noInfo k2 = Add (simplyfyValue knowlage v1) (simplyfyValue knowlage v2)    
    | isConst k1 && noInfo k2 = case c1 of
        0 -> SingleValue (simplyfyValue knowlage v2)
        _ -> Add (Const c1) (simplyfyValue knowlage v2)
    | noInfo k1 && isConst k2 = case c2 of
        0 -> SingleValue (simplyfyValue knowlage v1)
        _ -> Add (simplyfyValue knowlage v1) (Const c2)
    | otherwise = SingleValue (Const (c1+c2))
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
simplyfyExpersson knowlage (Sub v1 v2)
    | v1 == v2 = SingleValue (Const 0)
    | noInfo k1 && noInfo k2 = Sub (simplyfyValue knowlage v1) (simplyfyValue knowlage v2)
    | isConst k1 && noInfo k2 = case c1 of
        0 -> SingleValue (Const 0)
        _ -> Sub (Const c1) (simplyfyValue knowlage v2)
    | noInfo k1 && isConst k2 = case c2 of
        0 -> SingleValue (simplyfyValue knowlage v1)
        _ -> Sub (simplyfyValue knowlage v1) (Const c2)
    | c2 >= c1 = SingleValue (Const 0)
    | otherwise =  SingleValue (Const (c1-c2))
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
simplyfyExpersson knowlage (Mul v1 v2)
    | noInfo k1 && noInfo k2 = Mul (simplyfyValue knowlage v1) (simplyfyValue knowlage v2)
    | isConst k1 && noInfo k2 = case c1 of
        0 -> SingleValue (Const 0)
        1 -> SingleValue (simplyfyValue knowlage v2)
        _ -> Mul (Const c1) (simplyfyValue knowlage v2)
    | noInfo k1 && isConst k2 = case c2 of
        0 -> SingleValue (Const 0)
        1 -> SingleValue (simplyfyValue knowlage v1)
        _ -> Mul (simplyfyValue knowlage v1) (Const c2)
    | otherwise =  SingleValue (Const (c1*c2))
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
simplyfyExpersson knowlage (Div v1 v2)
    | noInfo k1 && noInfo k2 = Div (simplyfyValue knowlage v1) (simplyfyValue knowlage v2)    
    | isConst k1 && noInfo k2 = case c1 of
        0 -> SingleValue (Const 0)
        _ -> Div (Const c1) v2
    | noInfo k1 && isConst k2 = case c2 of
        0 -> SingleValue (Const 0)
        1 -> SingleValue (simplyfyValue knowlage v1)
        _ -> Div (simplyfyValue knowlage v1) (Const c2)
    | c2 == 0 = SingleValue (Const 0)
    | otherwise =  SingleValue (Const (c1 `div` c2))
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2
simplyfyExpersson knowlage (Mod v1 v2)
    | v1 == v2 = SingleValue (Const 0)
    | noInfo k1 && noInfo k2 = Mod (simplyfyValue knowlage v1) (simplyfyValue knowlage v2)
    | isConst k1 && noInfo k2 = case c1 of
        0 -> SingleValue (Const 0)
        _ -> Mod (Const c1) (simplyfyValue knowlage v2)
    | noInfo k1 && isConst k2 = case c2 of
        0 -> SingleValue (Const 0)
        1 -> SingleValue (Const 0)
        _ -> Mod (simplyfyValue knowlage v1) (Const c2)
    | c2 == 0 = SingleValue (Const 0)
    | otherwise =  SingleValue (Const (c1 `mod` c2))
        where
            k1 = getInfo knowlage v1
            k2 = getInfo knowlage v2
            Constant c1 = k1
            Constant c2 = k2

-- Computes resulet of comperason
constConditionVal :: Integer -> Comparator -> Integer -> Bool
constConditionVal a Translator.LE b = a <= b
constConditionVal a Translator.LT b = a < b
constConditionVal a Translator.EQ b = a == b
constConditionVal a Translator.NE b = a /= b
constConditionVal a Translator.GT b = a > b
constConditionVal a Translator.GE b = a >= b

-- In case of enought knowlage returns result of condition
solveCondition :: Knowlage -> Condition -> Maybe Bool
solveCondition k (Cond a op b)
    | isConst infoA && isConst infoB = Just $ constConditionVal a' op b'
    | isConst infoA && a' == 0 && op == Translator.GT = Just False
    | isConst infoB && b' == 0 && op == Translator.LT = Just False
    | isConst infoA && a' == 0 && op == Translator.LE = Just True
    | isConst infoB && b' == 0 && op == Translator.GE = Just True
    | otherwise = Nothing
    where
        infoA = getInfo k a
        infoB = getInfo k b
        Constant a' = infoA
        Constant b' = infoB

-- Replaces known value variables with consts
simplyfyCondition :: Knowlage -> Condition -> Condition
simplyfyCondition knowlage (Cond v1 op v2)
    | noInfo k1 && noInfo k2 = Cond (simplyfyValue knowlage v1) op (simplyfyValue knowlage v2)
    | v1 == v2 = Cond (Const 0) op (Const 0)
    | isConst k1 && noInfo k2 = Cond (Const c1) op (simplyfyValue knowlage v2)
    | noInfo k1 && isConst k2 = Cond (simplyfyValue knowlage v1) op (Const c2)
    | otherwise = if constConditionVal c1 op c2 then
            Cond (Const 0) Translator.EQ (Const 0)
        else
            Cond (Const 0) Translator.NE (Const 0)
    where
        k1 = getInfo knowlage v1
        k2 = getInfo knowlage v2
        Constant c1 = k1
        Constant c2 = k2

-- Generates state of knolage after executing list of istructions
computeKnowlage :: Knowlage -> [Command] -> Knowlage
computeKnowlage prev [] = prev
computeKnowlage prev (Assign id exp:rest) 
    = computeKnowlage (updateKnowlage prev id (infoAboutExpression prev exp)) rest
computeKnowlage prev (IfElse _ ins ins':rest) = computeKnowlage (combineKnowlage ifKnowlage elseKnowlage) rest
    where
        ifKnowlage = computeKnowlage prev ins
        elseKnowlage = computeKnowlage prev ins'
computeKnowlage prev (If _ ins:rest) = computeKnowlage (combineKnowlage ifKnowlage prev) rest
    where
        ifKnowlage = computeKnowlage prev ins
computeKnowlage prev (While _ ins:rest) = computeKnowlage (combineKnowlage loopKnowlage prev) rest
    where
        loopKnowlage = computeKnowlage prev ins
computeKnowlage prev (Until _ ins:rest) = computeKnowlage (combineKnowlage loopKnowlage prev) rest
    where
        loopKnowlage = computeKnowlage prev ins
computeKnowlage prev (ForUp _ _ _ ins:rest) = computeKnowlage (combineKnowlage loopKnowlage prev) rest
    where
        loopKnowlage = computeKnowlage prev ins
computeKnowlage prev (ForDown _ _ _ ins:rest) = computeKnowlage (combineKnowlage loopKnowlage prev) rest
    where
        loopKnowlage = computeKnowlage prev ins
computeKnowlage prev (Read id :rest) = computeKnowlage (updateKnowlage prev id Input) rest
computeKnowlage prev (Write _ :rest) = computeKnowlage prev rest 

-- Computes knowalge untli findes stable state
computeLoopKnolage :: Knowlage -> [Command] -> Knowlage
computeLoopKnolage prev ins
    | knowlage == prev = prev
    | otherwise = computeLoopKnolage knowlage ins
        where
            knowlage = combineKnowlage (computeKnowlage prev ins) prev

-- Using knolage about state of variables replaces known values with constants wherever is that possable
constFolding :: Knowlage -> [Command] -> [Command]
constFolding _ [] = []
constFolding knowlage (Assign id exp:rest) = Assign id' exp':constFolding knowlage' rest
    where
        infoExp = infoAboutExpression knowlage exp
        knowlage' = updateKnowlage knowlage id infoExp
        exp' = simplyfyExpersson knowlage exp
        id' = simplyfyId knowlage id
constFolding knowlage (IfElse cond ins ins':rest) = case solveCondition knowlage cond of
    Nothing -> IfElse foldedCond foldedIns foldedIns':constFolding combined rest
    Just True -> foldedIns ++ constFolding ifKnowlage rest
    Just False -> foldedIns ++ constFolding elseKnowlage rest
    where
        foldedIns = constFolding knowlage ins
        foldedIns' = constFolding knowlage ins'
        foldedCond = simplyfyCondition knowlage cond
        ifKnowlage = computeKnowlage knowlage ins
        elseKnowlage = computeKnowlage knowlage ins'
        combined = combineKnowlage ifKnowlage elseKnowlage
constFolding knowlage (If cond ins:rest) = case solveCondition knowlage cond of
    Nothing -> If foldedCond foldedIns:constFolding combined rest
    Just True -> foldedIns ++ constFolding ifKnowlage rest
    Just False -> constFolding knowlage rest
    where
        foldedIns = constFolding knowlage ins
        foldedCond = simplyfyCondition knowlage cond
        ifKnowlage = computeKnowlage knowlage ins
        combined = combineKnowlage ifKnowlage knowlage
constFolding knowlage (While cond ins:rest) = case solveCondition knowlage cond of
    Nothing -> While foldedCond foldedIns:constFolding knowlage' rest
    Just False -> constFolding knowlage rest
    Just True -> While foldedCond foldedIns:constFolding knowlage' rest
    where
        knowlage' = combineKnowlage knowlage (computeLoopKnolage knowlage ins)
        foldedCond = simplyfyCondition knowlage' cond
        foldedIns = constFolding knowlage' ins
constFolding knowlage (Until cond ins:rest) = case solveCondition knowlage' cond of
    Nothing ->  Until foldedCond foldedIns:constFolding knowlage' rest
    Just False -> Until foldedCond foldedIns:constFolding knowlage' rest
    Just True -> constFolding knowlage (ins++rest)
    where
        knowlage' = combineKnowlage knowlage (computeLoopKnolage knowlage ins)
        foldedCond = simplyfyCondition knowlage' cond
        foldedIns = constFolding knowlage' ins
constFolding knowlage (ForUp id v1 v2 ins: rest) = case solveCondition knowlage (Cond v1 Translator.LE v2) of 
    Nothing -> ForUp id foldedV1 foldedV2 foldedIns:constFolding knowlage' rest
    Just True -> ForUp id foldedV1 foldedV2 foldedIns:constFolding knowlage' rest
    Just False -> constFolding knowlage rest
    where
        foldedV1 = simplyfyValue knowlage v1
        foldedV2 = simplyfyValue knowlage v2
        foldedIns = constFolding knowlage' ins
        loopKnowlage = computeLoopKnolage knowlage ins
        knowlage' = combineKnowlage knowlage loopKnowlage
constFolding knowlage (ForDown id v1 v2 ins: rest) = case solveCondition knowlage (Cond v1 Translator.GE v2) of 
    Nothing -> ForDown id foldedV1 foldedV2 foldedIns:constFolding knowlage' rest
    Just True -> ForDown id foldedV1 foldedV2 foldedIns:constFolding knowlage' rest
    Just False -> constFolding knowlage rest
    where
        foldedV1 = simplyfyValue knowlage v1
        foldedV2 = simplyfyValue knowlage v2
        foldedIns = constFolding knowlage' ins
        loopKnowlage = computeLoopKnolage knowlage ins
        knowlage' = combineKnowlage knowlage loopKnowlage
constFolding knowlage (Write v:rest) = Write foldedV:constFolding knowlage rest
    where
        foldedV = simplyfyValue knowlage v
constFolding knowlage (Read id:rest) = Read id':constFolding knowlage' rest
    where
        knowlage' = updateKnowlage knowlage id Input
        id' = simplyfyId knowlage id

-- Replaces known value variables 
foldConstants :: [Command] -> [Command]
foldConstants = constFolding empty 




