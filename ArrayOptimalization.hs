module ArrayOptimalization where

import Parser
import qualified Data.List as List
import Debug.Trace
import Debug

valueDynamicArray :: Value -> [String]
valueDynamicArray (Var (ArrayVar name _)) = [name]
valueDynamicArray _ = []

expressionDynamicArray :: Expression -> [String]
expressionDynamicArray exp = case exp of
    SingleValue v -> valueDynamicArray v
    Add v1 v2 -> valueDynamicArray v1 `List.union` valueDynamicArray v2
    Sub v1 v2 -> valueDynamicArray v1 `List.union` valueDynamicArray v2
    Mul v1 v2 -> valueDynamicArray v1 `List.union` valueDynamicArray v2
    Div v1 v2 -> valueDynamicArray v1 `List.union` valueDynamicArray v2
    Mod v1 v2 -> valueDynamicArray v1 `List.union` valueDynamicArray v2

conditionDynamicArray :: Condition -> [String]
conditionDynamicArray (Cond v1 _ v2) = valueDynamicArray v1 `List.union` valueDynamicArray v2

commandsDynamicArray :: [String] -> [Command] -> [String]
commandsDynamicArray acc [] = acc
commandsDynamicArray acc (Assign id exp:rest) = case id of
    ArrayVar name _ -> commandsDynamicArray ([name] `List.union` expressionDynamicArray exp `List.union` acc) rest
    _ -> commandsDynamicArray (expressionDynamicArray exp `List.union` acc) rest
commandsDynamicArray acc (If cond ins:rest) = commandsDynamicArray (acc `List.union` conditionDynamicArray cond `List.union` commandsDynamicArray [] ins) rest
commandsDynamicArray acc (IfElse cond ins ins':rest)
 = commandsDynamicArray (acc `List.union` conditionDynamicArray cond `List.union` commandsDynamicArray [] ins `List.union` commandsDynamicArray [] ins') rest
commandsDynamicArray acc (While cond ins:rest) = commandsDynamicArray (acc `List.union` conditionDynamicArray cond `List.union` commandsDynamicArray [] ins) rest
commandsDynamicArray acc (Until cond ins:rest) = commandsDynamicArray (acc `List.union` conditionDynamicArray cond `List.union` commandsDynamicArray [] ins) rest
commandsDynamicArray acc (ForUp _ v1 v2 ins:rest) = commandsDynamicArray (acc `List.union` valueDynamicArray v1 `List.union` valueDynamicArray v2 `List.union` commandsDynamicArray [] ins) rest
commandsDynamicArray acc (ForDown _ v1 v2 ins:rest) = commandsDynamicArray (acc `List.union` valueDynamicArray v1 `List.union` valueDynamicArray v2 `List.union` commandsDynamicArray [] ins) rest
commandsDynamicArray acc (Write v:rest) = commandsDynamicArray (acc `List.union` valueDynamicArray v) rest
commandsDynamicArray acc (Read id:rest) = case id of
    ArrayVar name _ -> commandsDynamicArray ([name] `List.union` acc) rest
    _ -> commandsDynamicArray acc rest

shiftId :: String -> Integer -> Identifier -> Identifier
shiftId name shift (ArrayConst n p )
    | n == name = ArrayConst n (p-shift)
    | otherwise = ArrayConst n p
shiftId _ _ x = x

shiftVal name shift (Var id) = Var (shiftId name shift id)
shiftVal _ _ x = x

shiftExp :: String -> Integer -> Expression -> Expression
shiftExp name shift exp = case exp of
    SingleValue v -> SingleValue (shiftVal name shift v)
    Add v1 v2 -> Add (shiftVal name shift v1) (shiftVal name shift v2)
    Sub v1 v2 -> Sub (shiftVal name shift v1) (shiftVal name shift v2)
    Mul v1 v2 -> Mul (shiftVal name shift v1) (shiftVal name shift v2)
    Div v1 v2 -> Div (shiftVal name shift v1) (shiftVal name shift v2)
    Mod v1 v2 -> Mod (shiftVal name shift v1) (shiftVal name shift v2)

shiftCond :: String -> Integer -> Condition -> Condition
shiftCond name shift (Cond v1 op v2) = Cond (shiftVal name shift v1) op (shiftVal name shift v2)

shiftCommands :: [Command] -> String -> Integer -> [Command] -> [Command]
shiftCommands acc _ _ [] = reverse acc
shiftCommands acc name pos (Assign id exp:rest) 
    = shiftCommands (Assign (shiftId name pos id) (shiftExp name pos exp):acc) name pos rest
shiftCommands acc name pos (If cond ins:rest)
    = shiftCommands (If (shiftCond name pos cond) (shiftCommands [] name pos ins):acc) name pos rest
shiftCommands acc name pos (IfElse cond ins ins':rest)
    = shiftCommands (IfElse (shiftCond name pos cond) (shiftCommands [] name pos ins) (shiftCommands [] name pos ins'):acc) name pos rest
shiftCommands acc name pos (Until cond ins:rest)
    = shiftCommands (Until (shiftCond name pos cond) (shiftCommands [] name pos ins):acc) name pos rest
shiftCommands acc name pos (While cond ins:rest)
    = shiftCommands (While (shiftCond name pos cond) (shiftCommands [] name pos ins):acc) name pos rest
shiftCommands acc name pos (ForUp n v1 v2 ins:rest)
    = shiftCommands (ForUp n (shiftVal name pos v1) (shiftVal name pos v2) (shiftCommands [] name pos ins):acc) name pos rest
shiftCommands acc name pos (ForDown n v1 v2 ins:rest) 
    = shiftCommands (ForDown n (shiftVal name pos v1) (shiftVal name pos v2) (shiftCommands [] name pos ins):acc) name pos rest
shiftCommands acc name pos (Read id:rest)
    = shiftCommands (Read (shiftId name pos id):acc) name pos rest
shiftCommands acc name pos (Write v:rest)
    = shiftCommands (Write (shiftVal name pos v):acc) name pos rest

t declarations ins = g
    where
        g = commandsDynamicArray [] ins

forShift :: [Command] -> [Command] -> [Command]
forShift acc [] = reverse acc
forShift acc (ins@(Assign _ _):xs) = forShift (ins:acc) xs
forShift acc (ins@(Write _):xs) = forShift (ins:acc) xs
forShift acc (ins@(Read _):xs) = forShift (ins:acc) xs
forShift acc (IfElse cond ins ins':xs) = forShift (IfElse cond (forShift [] ins) (forShift [] ins'):acc) xs
forShift acc (If cond ins:xs) = forShift (If cond (forShift [] ins):acc) xs
forShift acc (Until cond ins:xs) = forShift (Until cond (forShift [] ins):acc) xs
forShift acc (While cond ins:xs) = forShift (While cond (forShift [] ins):acc) xs
forShift acc (ForUp n v1 v2 ins:rest)
    = if head n == '*' && isConst v1 && isConst v2 && c1 <= c2
    then forShift (ForUp n (Const 0) (Const (c2-c1)) (forShift [] ins):acc) rest
    else forShift (ForUp n v1 v2 ins:acc) rest
    where
        isConst (Const _) = True 
        isConst _ = False
        Const c1 = v1
        Const c2 = v2
forShift acc (ForDown n v1 v2 ins:rest) 
    = if head n == '*' && isConst v1 && isConst v2 && c1 >= c2
    then forShift (ForDown n (Const (c1-c2)) (Const 0) (forShift [] ins):acc) rest
    else forShift (ForDown n v1 v2 ins:acc) rest
    where
        isConst (Const _) = True 
        isConst _ = False
        Const c1 = v1
        Const c2 = v2        

optimizeArrays :: [Declaration] -> [Command] -> ([Declaration], [Command])
optimizeArrays declarations ins = (declarations', ins')
    where
        arrays = List.filter isArray declarations
        isArray ArrayDeclaration {} = True
        isArray _ = False
        name (ArrayDeclaration n _ _) = n
        name (SingleDeclaration n) = n
        staticArrays = (name <$> arrays) List.\\ commandsDynamicArray [] ins
        staticDeclarations = (\x -> head (filter (\y -> name y == x) arrays)) <$> staticArrays
        ins' = forShift [] shiftedIns
        (shiftedArrays, shiftedIns) = List.foldl go ([], ins) staticDeclarations
        declarations' = (declarations List.\\ staticDeclarations) `List.union` shiftedArrays
        go :: ([Declaration], [Command]) -> Declaration -> ([Declaration], [Command])
        go (prevDec, prevIns) (ArrayDeclaration n f t) = (ArrayDeclaration n 0 (t-f):prevDec, shiftCommands [] n f prevIns)
