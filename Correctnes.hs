module Correctnes(checkCorrectness) where

import Parser
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace
import Debug

join :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
join (a, b) (c, d) = (a++c, b++d)

usedInId :: Identifier -> ([String], [String])
usedInId (SingleVar n) = ([n], [])
usedInId (ArrayConst n _) = ([], [n])
usedInId (ArrayVar a b) = ([b], [a])

usedInVal :: Value -> ([String], [String])
usedInVal (Const _) = ([], [])
usedInVal (Var id) = usedInId id

usedInCond :: Condition -> ([String], [String])
usedInCond (Cond v1 _ v2) = join (usedInVal v1) (usedInVal v2)

usedInExp :: Expression -> ([String], [String])
usedInExp exp = case exp of
    SingleValue v -> usedInVal v
    Add v1 v2 -> usedInVal v1 `join` usedInVal v2 
    Sub v1 v2 -> usedInVal v1 `join` usedInVal v2
    Mul v1 v2 -> usedInVal v1 `join` usedInVal v2
    Div v1 v2 -> usedInVal v1 `join` usedInVal v2
    Mod v1 v2 -> usedInVal v1 `join` usedInVal v2

convertDeclarations :: [Declaration] -> Map.Map String Bool -> Map.Map String (Integer, Integer) -> Either [Char] (Map.Map String Bool, Map.Map String (Integer, Integer))
convertDeclarations [] acc1 acc2 = Right (acc1, acc2)
convertDeclarations (SingleDeclaration str:xs) acc1 acc2 
    | Map.member str acc1 = Left (str++" is already declared")
    | Map.member str acc2 = Left (str++" there exist already array with this same name")
    | otherwise = convertDeclarations xs (Map.insert str False acc1) acc2
convertDeclarations (ArrayDeclaration str range1 range2:xs) acc1 acc2 
    | range1 > range2 = Left (str++" has wrong ranges")
    | Map.member str acc1 = Left (str++" there exist already variable with this same name")
    | Map.member str acc2 = Left (str++" this array is already declared")
    | otherwise = convertDeclarations xs acc1 (Map.insert str (range1, range2) acc2)

checkCorrectness :: [Declaration] -> [Command] -> Either [Char] ([Declaration], [Command])
checkCorrectness declarations commands =  do
    (single, array) <- convertDeclarations declarations Map.empty Map.empty
    (commands', _) <- check commands [] Set.empty single array
    return (declarations, commands')


check :: [Command] -> [Command] -> Set.Set String -> Map.Map String Bool -> Map.Map String a -> Either [Char] ([Command], Map.Map String Bool)
check [] acc readOnly single array = Right (reverse acc, single)
check (Assign id exp:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_exp = Left ("Used undelcared variable - "++show (Assign id exp))
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_exp = Left ("Used uninitialized variable - "++show (Assign id exp))
    | any (\x -> not (Map.member x array)) array_exp = Left ("Used undefined array - "++show (Assign id exp))
    | (single_id /= []) && null array_id && head single_id `elem` readOnly = Left ("Tried to changed iterator - "++show (Assign id exp))
    | (single_id /= []) && not (head single_id `Set.member` good_single) = Left ("Tried to save to unexisting variable - "++show (Assign id exp))
    | (array_id /= []) && not (head array_id `Map.member` array) = Left ("Tried to save to unexisting array - "++show (Assign id exp))
    | single_id /= [] && null array_id= check xs (Assign id exp:acc) readOnly (Map.insert(head single_id) True single) array
    | otherwise = check xs (Assign id exp:acc) readOnly single array
    where
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_exp, array_exp) = usedInExp exp
        (single_id, array_id) = usedInId id
check (IfElse cond ins ins2:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_cond = Left ("Used undelcared variable - "++show cond)
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_cond = Left ("Used uninitialized variable - "++show cond)
    | any (\x -> not (Map.member x array)) array_cond = Left ("Used undefined array - " ++show cond)
    | otherwise = res
    where 
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_cond, array_cond) = usedInCond cond
        res = do
            (ins', single') <- check ins [] readOnly single array
            (ins2', single2') <- check ins2 [] readOnly single array
            let new_single = Map.unionWith (&&) single' single2'
            check xs (IfElse cond ins' ins2':acc) readOnly new_single array
check (If cond ins:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_cond = Left ("Used undelcared variable - " ++show cond)
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_cond = Left ("Used uninitialized variable - " ++show cond)
    | any (\x -> not (Map.member x array)) array_cond = Left ("Used undefined array - " ++show cond) 
    | otherwise = res
    where 
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_cond, array_cond) = usedInCond cond
        res = do
            (ins', single') <- check ins [] readOnly single array
            check xs (If cond ins' :acc) readOnly single' array
check (While cond ins:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_cond = Left ("Used undelcared variable - " ++show cond)
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_cond = Left ("Used uninitialized variable - " ++show cond)
    | any (\x -> not (Map.member x array)) array_cond = Left ("Used undefined array - " ++show cond) 
    | otherwise = res
    where 
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_cond, array_cond) = usedInCond cond
        res = do
            (ins', single') <- check ins [] readOnly single array
            check xs (While cond ins' :acc) readOnly single' array 
check (Until cond ins:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_cond = Left ("Used undelcared variable - " ++show cond)
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_cond = Left ("Used uninitialized variable - " ++show cond)
    | any (\x -> not (Map.member x array)) array_cond = Left ("Used undefined array - " ++show cond) 
    | otherwise = res
    where 
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_cond, array_cond) = usedInCond cond
        res = do
            (ins', single') <- check ins [] readOnly single array
            check xs (Until cond ins' :acc) readOnly single' array 
check (ForUp str v1 v2 ins:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_cond = Left ("Used undelcared variable - " ++show (ForUp str v1 v2 ins))
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_cond = Left ("Used uninitialized variable - " ++show (ForUp str v1 v2 ins))
    | any (\x -> not (Map.member x array)) array_cond = Left ("Used undefined array - " ++show (ForUp str v1 v2 ins)) 
    | otherwise = res
    where 
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_cond, array_cond) = usedInVal v1 `join` usedInVal v2
        res = do
            (ins', single') <- check ins [] (Set.insert str readOnly) single array
            check xs (ForUp str v1 v2 ins':acc) readOnly single' array
check (ForDown str v1 v2 ins:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_cond = Left ("Used undelcared variable - " ++show (ForDown str v1 v2 ins)) 
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_cond = Left ("Used uninitialized variable - " ++show (ForDown str v1 v2 ins)) 
    | any (\x -> not (Map.member x array)) array_cond = Left ("Used undefined array - " ++show (ForDown str v1 v2 ins)) 
    | otherwise = res
    where 
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_cond, array_cond) = usedInVal v1 `join` usedInVal v2
        res = do
            (ins', single') <- check ins [] (Set.insert str readOnly) single array
            check xs (ForDown str v1 v2 ins':acc) readOnly single' array 
check (Read id:xs) acc readOnly single array
    | (single_id /= []) && null array_id && head single_id `elem` readOnly = Left ("Tried to changed iterator - "++show (Read id))
    | (single_id /= []) && not (head single_id `Set.member` good_single) = Left ("Tried to save to unexisting variable - "++show (Read id))
    | (array_id /= []) && not (head array_id `Map.member` array) = Left ("Tried to save to unexisting array - "++show (Read id))
    | single_id /= [] = check xs (Read id:acc) readOnly (Map.insert(head single_id) True single) array
    | otherwise = check xs (Read id:acc) readOnly single array
    where
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_id, array_id) = usedInId id 
check (Write v:xs) acc readOnly single array
    | any (\x -> not $ Set.member x good_single) single_val = Left ("Used undelcared variable - "++show (Write v))
    | any (\x -> not (Set.member x readOnly || (single Map.! x))) single_val = Left ("Used uninitialized variable - "++show (Write v))
    | any (\x -> not (Map.member x array)) array_val = Left ("Used undefined array - "++show (Write v))
    | otherwise = check xs (Write v:acc) readOnly single array
    where
        good_single = readOnly `Set.union` Set.fromList (fst <$> Map.toList single)
        (single_val, array_val) = usedInVal v
