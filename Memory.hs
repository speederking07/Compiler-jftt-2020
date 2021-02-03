module Memory where
import Data.Map.Strict (toList, empty, insert, fromList, Map, insertWith)
import qualified Data.List as List
import Parser

import Debug.Trace
import Debug 

type MemoryStructure = Map String Integer 

msort :: Ord b => (a->b) -> [a] -> [a]
msort _ [] = []
msort _ [a] = [a]
msort f xs = merge f (msort f firstHalf) (msort f secondHalf)
    where
        (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
        merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
        merge _ xs [] = xs
        merge _ [] ys = ys
        merge f (x:xs) (y:ys) | f x >= f y = x:merge f xs (y:ys)
                              | otherwise = y:merge f (x:xs) ys

-- List of numbers in order of cost to generate
listBestFits :: [Integer]
listBestFits = concat $ [0]:[1]:f [1]
    where
        f :: [Integer] -> [[Integer]]
        f a = new : f new
            where
                new = a >>= i
        i :: Integer -> [Integer]
        i n
            | even n = [n*2, n+1]
            | otherwise = [n*2]

-- Finds best address for specific declaration
findBestFit :: Declaration -> [(Integer, Integer)] -> (Integer, Integer)
findBestFit d r = case d of 
    SingleDeclaration _ ->  (findFirst listBestFits (\x -> fitsRestrictions x x r), findFirst listBestFits (\x -> fitsRestrictions x x r))
    ArrayDeclaration _ b e -> (findFirst listBestFits (\x -> fitsRestrictions (x+b) (x+e) r), findFirst listBestFits (\x -> fitsRestrictions (x+b) (x+e) r) + e)
    where
        findFirst :: [a] -> (a -> Bool) -> a
        findFirst (x:xs) f
            | f x = x
            | otherwise = findFirst xs f
        fitsRestriction from to (a, b) 
            | from > b || to < a = True
            | otherwise = False
        fitsRestrictions _ _ [] = True
        fitsRestrictions from to (r:xr)
            | fitsRestriction from to r = fitsRestrictions from to xr
            | otherwise = False
        
-- Generacts memory allocation structur for specific list of declarations in order of most usefull 
getMemoryAllocation :: [Declaration] -> MemoryStructure
getMemoryAllocation = go [] []
    where
        go acc r (d:ds) = go ((name d, i1):acc) ((i1, i2):r) ds  
            where
                (i1, i2) = findBestFit d r
        go acc _ [] = fromList acc
        name (SingleDeclaration a) = a
        name (ArrayDeclaration a _ _) = a

-- replaces identificator in list of instruction 
replaceIdentifier :: String -> String -> [Command] -> [Command]
replaceIdentifier from to = fmap (replaceCommand from to)
    where
        replaceCommand from to comm = case comm of 
            Assign id exp -> Assign (replaceId from to id) (replaceExpresson from to exp)
            IfElse cond ins1 ins2 -> IfElse (replaceCondition from to cond) (replaceIdentifier from to ins1) (replaceIdentifier from to ins2)
            If cond ins -> If (replaceCondition from to cond) (replaceIdentifier from to ins)
            While cond ins -> While (replaceCondition from to cond) (replaceIdentifier from to ins)
            Until cond ins -> Until (replaceCondition from to cond) (replaceIdentifier from to ins)
            ForUp dec v1 v2 ins -> ForUp dec (replaceValue from to v1) (replaceValue from to v2) (replaceIdentifier from to ins)
            ForDown dec v1 v2 ins -> ForDown dec (replaceValue from to v1) (replaceValue from to v2) (replaceIdentifier from to ins)
            Read id -> Read (replaceId from to id)
            Write v -> Write (replaceValue from to v)
        replaceExpresson from to exp = case exp of 
            SingleValue v1 -> SingleValue (replaceValue from to v1)
            Add v1 v2 -> Add (replaceValue from to v1) (replaceValue from to v2)
            Sub v1 v2 -> Sub (replaceValue from to v1) (replaceValue from to v2)
            Mul v1 v2 -> Mul (replaceValue from to v1) (replaceValue from to v2)
            Div v1 v2 -> Div (replaceValue from to v1) (replaceValue from to v2)
            Mod v1 v2 -> Mod (replaceValue from to v1) (replaceValue from to v2)
        replaceCondition from to (Cond v1 c v2) = Cond (replaceValue from to v1) c (replaceValue from to v2)
        replaceValue from to val = case val of
            Const int -> Const int
            Var id -> Var (replaceId from to id)
        replaceId from to id = case id of
            SingleVar str -> SingleVar (replaceStr from to str)
            ArrayConst str int -> ArrayConst (replaceStr from to str) int
            ArrayVar str1 str2 -> ArrayVar (replaceStr from to str1) (replaceStr from to str2)
        replaceStr from to curr
            | from == curr = to
            | otherwise = curr

-- Pulls out variables declared in for loops and replaces them with generic -loopn 
pullOutForDeclarations :: [Command] -> ([Declaration], [Command])
pullOutForDeclarations commands = (generateDec m', c')
    where
        (m', c') = go 0 0 [] commands
        go :: Int -> Int -> [Command] -> [Command] -> (Int, [Command])        
        go _ max commands [] = (max, reverse commands)
        go level max acc (Assign id exp:rest) = go level max (Assign id exp:acc) rest
        go level max acc (IfElse cond ins1 ins2:rest) = go level max' (IfElse cond i1 i2:acc) rest 
            where 
                max' = Prelude.max m1 m2
                (m1, i1) = go level max [] ins1
                (m2, i2) = go level max [] ins2
        go level max acc (If cond ins:rest) = go level max' (If cond ins':acc) rest 
            where 
                (max', ins') = go level max [] ins
        go level max acc (While cond ins:rest) = go level max' (While cond ins':acc) rest 
            where 
                (max', ins') = go level max [] ins
        go level max acc (Until cond ins:rest) = go level max' (Until cond ins':acc) rest 
            where 
                (max', ins') = go level max [] ins
        go level max acc (Read id:rest) = go level max (Read id:acc) rest 
        go level max acc (Write val:rest) = go level max (Write val:acc) rest
        go level max acc (ForUp dec v1 v2 ins:rest) = go level (Prelude.max max' (level+1)) (ForUp dec' v1 v2 ins':acc) rest 
            where 
                --TODO Rak
                (max', ins'') = go (level+1) max [] ins
                dec' = if test' /= test then "-loop"++show (level+1) else "*"++show (level+1)
                test = replaceIdentifier dec "%%" ins''
                test' = replaceIdentifier dec "%" ins''
                ins' = if ins /= test then replaceIdentifier dec dec' ins'' else ins
        go level max acc (ForDown dec v1 v2 ins:rest) = go level (Prelude.max max' (level+1)) (ForDown dec' v1 v2 ins':acc) rest 
            where 
                (max', ins'') = go (level+1) max [] ins
                dec' = if test' /= test then "-loop"++show (level+1) else "*"++show (level+1)
                test = replaceIdentifier dec "%%" ins''
                test' = replaceIdentifier dec "%" ins''
                ins' = if ins /= test then replaceIdentifier dec dec' ins'' else ins 
        generateDec 0 = []
        generateDec n = SingleDeclaration ("*"++show n++"'") : SingleDeclaration ("*"++show n) : SingleDeclaration ("-loop"++show n++"'") : SingleDeclaration ("-loop"++show n) : generateDec (n-1)


sortByUsefullnes :: [Command] -> [Declaration] -> [Declaration]
sortByUsefullnes commands dec = result
    where
        zero_map = foldl (\a b -> insert (name b) 0 a) empty dec
        weight_map = toList $ usefulness weight zero_map commands
        list_of_dec = (\(s, w) -> (get (\n -> name n == s) dec, w)) <$> List.filter (\(_, w) -> w /= 0) weight_map
        result = fst <$> msort (\(d, w) -> fromInteger w / fromInteger (size d)) list_of_dec
        size (SingleDeclaration _) = 1
        size (ArrayDeclaration _ a b) = b - a
        name (SingleDeclaration n) = n
        name (ArrayDeclaration n _ _) = n
        get f (x:xs)
            | f x = x
            | otherwise = get f xs
        usedInVal (Const _) = []
        usedInVal (Var id) = usedInId id
        usedInId (SingleVar a) = [a]
        usedInId (ArrayConst a _) = [a]
        usedInId (ArrayVar a b) = [a, b]
        usedInExp exp = case exp of
            SingleValue v -> usedInVal v
            Add a b -> usedInVal a ++ usedInVal b
            Sub a b -> usedInVal a ++ usedInVal b
            Mul a b -> usedInVal a ++ usedInVal b
            Div a b -> usedInVal a ++ usedInVal b
            Mod a b -> usedInVal a ++ usedInVal b
        usedInCond (Cond a op b) = usedInVal a ++ usedInVal b
        applyList :: Integer -> [String] -> Map String Integer -> Map String Integer 
        applyList level list map = foldl (\m a -> insertWith (+) a level m) map list
        weight = 5
        usefulness :: Integer -> Map String Integer -> [Command] -> Map String Integer
        usefulness _ map [] = map
        usefulness level map (x:xs) = case x of
            Assign id exp -> usefulness level (applyList level (usedInId id ++ usedInExp exp) map) xs
            IfElse cond ins ins' ->  usefulness level used3 xs
                where
                    used1 = usefulness level map ins 
                    used2 = usefulness level used1 ins' 
                    used3 = applyList level (usedInCond cond) used2 
            If cond ins ->  usefulness level used2 xs
                where
                    used1 = usefulness level map ins 
                    used2 = applyList level (usedInCond cond) used1
            While cond ins ->  usefulness level used2 xs
                where
                    used1 = usefulness (level*weight) map ins 
                    used2 = applyList level (usedInCond cond) used1 
            Until cond ins ->  usefulness level used2 xs
                where
                    used1 = usefulness (level*weight) map ins 
                    used2 = applyList level (usedInCond cond) used1 
            ForUp str v1 v2 ins ->  usefulness level used2 xs
                where
                    used1 = usefulness (level*weight) map ins 
                    id = case str of
                        '*':rest -> "-loop"++rest++"'"
                        oth -> oth++"'"
                    used2 = applyList (level*weight) [id] $ applyList level (usedInVal v1 ++ usedInVal v2) used1 
            ForDown str v1 v2 ins -> usefulness level used2 xs
                where
                    used1 = usefulness (level*weight) map ins 
                    id = case str of
                        '*':rest -> "-loop"++rest++"'"
                        oth -> oth++"'"
                    used2 = applyList (level*weight) [id] $ applyList level (usedInVal v1 ++ usedInVal v2) used1 
            Read id -> usefulness level (applyList level (usedInId id) map) xs
            Write val -> usefulness level (applyList level (usedInVal val) map) xs

