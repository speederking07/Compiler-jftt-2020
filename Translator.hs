module Translator where

import Data.List (genericLength)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import qualified Data.List as List

data Register = A | B | C | D | E | F deriving (Eq, Ord)

instance Show Register where
    show A = "a"
    show B = "b"
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"

data Output
    = GET Register                 -- pobraną liczbę zapisuje w komórce pamięci prx oraz k ← k + 1     100
    | PUT Register                 -- wyświetla zawartość komórki pamięci prx oraz k ← k + 1           100
    | LOAD Register Register       -- rx ← pry oraz k ← k + 1                                           20
    | STORE Register Register      -- pry ← rx oraz k ← k + 1                                           20
    | ADD Register Register        -- rx ← rx + ry oraz k ← k + 1                                        5
    | SUB Register Register        -- rx ← max{rx − ry, 0} oraz k ← k + 1                                5
    | RESET Register               -- rx ← 0 oraz k ← k + 1                                              1
    | INC Register                 -- rx ← rx + 1 oraz k ← k + 1                                         1
    | DEC Register                 -- rx ← max(rx − 1, 0) oraz k ← k + 1                                 1
    | SHR Register                 -- rx ← floor(rx/2) oraz k ← k + 1                                    1
    | SHL Register                 -- rx ← rx ∗ 2 oraz k ← k + 1                                         1
    | JUMP Integer                 -- k ← k + j                                                          1
    | JZERO Register Integer       -- jeśli rx = 0 to k ← k + j, w p.p. k ← k + 1                        1
    | JODD Register Integer        -- jeśli rx nieparzyste to k ← k + j, w p.p. k ← k + 1                1
    | HALT                         -- zatrzymaj program                                                  0
    | COMMENT String
    deriving (Eq)

instance Show Output where
    show (GET r) = "GET "++ show r    
    show (PUT r) = "PUT " ++ show r                   
    show (LOAD r r') = "LOAD " ++ show r ++ " " ++ show r'         
    show (STORE r r') = "STORE " ++ show r ++ " " ++ show r'         
    show (ADD r r') = "ADD " ++ show r ++ " " ++ show r'                                                
    show (SUB r r') = "SUB " ++ show r ++ " " ++ show r'                                       
    show (RESET r) = "RESET " ++ show r                 
    show (INC r) = "INC " ++ show r                                                          
    show (DEC r) = "DEC " ++ show r                       
    show (SHR r) = "SHR " ++ show r 
    show (SHL r) = "SHL " ++ show r 
    show (JUMP n) 
        | n < 0 = "JUMP -" ++ show (-n) 
        | otherwise = "JUMP " ++ show n                
    show (JZERO r n)
        | n < 0 = "JZERO " ++ show r ++ " -" ++ show (-n) 
        | otherwise = "JZERO " ++ show r ++ " " ++ show n                  
    show (JODD r n)
        | n < 0 = "JODD " ++ show r ++ " -" ++ show (-n) 
        | otherwise = "JODD " ++ show r ++ " " ++ show n          
    show HALT =  "HALT" 
    show (COMMENT str) = '(':str++")"

data Comparator = LE | LT | EQ | NE | GT | GE deriving (Eq)

-- Generates opposite comperator
oppositeComperator :: Comparator -> Comparator
oppositeComperator comp = case comp of
    Translator.LE -> Translator.GT
    Translator.LT -> Translator.GE
    Translator.EQ -> Translator.NE
    Translator.NE -> Translator.EQ
    Translator.GT -> Translator.LE
    Translator.GE -> Translator.LT

-- Resturns cost of instruction
cost :: Num p => Output -> p
cost output = case output of
    GET _ -> 100
    PUT _ -> 100              
    LOAD _ _ -> 20   
    STORE _ _ -> 20
    ADD _ _ -> 5    
    SUB _ _ -> 5    
    RESET _ -> 1           
    INC _ -> 1            
    DEC _ -> 1          
    SHR _ -> 1             
    SHL _ -> 1             
    JUMP _ -> 1              
    JZERO _ _ -> 1    
    JODD _ _ -> 1     
    HALT -> 0  
    COMMENT _ -> 0

-- Restuns length of instructions does not count comments
insLength :: (Foldable t, Num a, Functor t) => t Output -> a
insLength l = sum (jump <$> l)
    where
        jump (COMMENT _) = 0
        jump _ = 1

-- Converts [Output] into string for VM
prettyShow :: [Output] -> String 
prettyShow l = concat $ sh <$> l
    where
        sh (COMMENT s) = " " ++ show (COMMENT s)
        sh c = "\n" ++ show c

-- Computes log_2 of n 
log_2Ceil :: Integer -> Integer
log_2Ceil n = genericLength (takeWhile (<n) (iterate (*2) 1))

-- Is power of 2
getPowerOfTwo :: Integer -> Maybe Integer
getPowerOfTwo n 
    | n == 2 ^ log_2Ceil n = Just $ log_2Ceil n
    | otherwise = Nothing

-- Converts binary string from lest meanig number to number
convert :: Num a => [Char] -> a
convert [] = 0
convert ('1' : xs) = 1 + 2 * convert xs
convert (_ : xs) = 2 * convert xs

-- computes costs of instraction list
computeCosts :: (Foldable t, Num b, Functor t) => t Output -> b
computeCosts list = sum (cost <$> list)  

-- Choose best option to generate const knowing previes state
bestGenerateConst :: Integer -> Integer -> Register -> [Output]
bestGenerateConst prev c r
    | Prelude.abs (prev - c) <= cost = if prev > c
        then List.genericTake (prev-c) $ repeat (DEC r)
        else List.genericTake (c - prev) $ repeat (INC r) 
    | computeCosts simple <= computeCosts convert = simple
    | otherwise = convert
    where
        cost = Prelude.min (computeCosts simple) (computeCosts convert)
        simple = generateConsFromBegin c r
        convert = generateConstFromConst prev c r

-- generating const in register, assuming there is 0
generateConst :: Integer -> Register -> [Output]
generateConst n r = go [] n
    where
        go acc 0 = acc
        go acc 1 = INC r:acc
        go acc n
            | even n = go (SHL r:acc) (n `div` 2)
            | otherwise = go (SHL r:INC r:acc) (n `div` 2)

-- generating const in register from scratch
generateConsFromBegin :: Integer -> Register -> [Output]
generateConsFromBegin n r = RESET r : generateConst n r

-- uses previous const to generate new one in its place 
generateConstFromConst :: (Integral a, Show a) => a -> Integer -> Register -> [Output]
generateConstFromConst 0 c r = generateConst c r
generateConstFromConst prev c r = replicate toReduce (SHR r) ++ (drop usefullBits bitNew >>= bitsToOutput r)
    where
        usefullBits = sameFront bitPrev bitNew
        toReduce = genericLength bitPrev - usefullBits
        bitPrev = showIntAtBase 2 intToDigit prev ""
        bitNew = showIntAtBase 2 intToDigit c ""
        sameFront [] _ = 0
        sameFront _ [] = 0
        sameFront (x:xs) (y:ys)
            | x == y = 1 + sameFront xs ys
            | otherwise = 0
        bitsToOutput r '1' = [SHL r, INC r]
        bitsToOutput r '0' = [SHL r]

-- multiply a * b, better if b < a, and saves in res
multiplyRegisters :: Register -> Register -> Register -> [Output]
multiplyRegisters b a res = [
    COMMENT "MUL begins",
    JODD a 5,
    JZERO a 9,
    SHL b,   
    SHR a,
    JUMP (-4),
    ADD res b,
    SHR a,
    JZERO a 3,
    SHL b,    
    JUMP (-9)]

-- Multiply register a by const and saves restult in res. Register a is lost
multiplyByConst :: Register -> Register -> Integer -> [Output]
multiplyByConst a res = go
    where
        go 0 = []
        go n
            | even n = SHL a: go (n `div` 2)
            | otherwise = ADD res a:SHL a:go (n `div` 2)


-- divides a/b and saves in res, for comperason purpeses needs tmp, leves value only in res (res should be 0)
divideRegisters :: Register -> Register -> Register -> Register -> [Output]
divideRegisters a b res tmp = [
    COMMENT "DIV begins",
    JZERO b 23,
    JODD b 4,
    SHR a,
    SHR b,
    JUMP (-3),
    RESET tmp,
    ADD tmp b,
    SUB tmp a,
    JZERO tmp 2,
    JUMP 3,
    SHL b,
    JUMP (-6),
    JODD b 11,
    SHL res,
    SHR b,
    RESET tmp,
    ADD tmp b,
    SUB tmp a,
    JZERO tmp 2,
    JUMP (-7),
    SUB a b,
    INC res,
    JUMP (-10)]

-- Computes a%b and saves result in a, strach in others registers (c should be 0)
moduloRegisters :: Register -> Register -> Register -> Register -> [Output]
moduloRegisters a b c tmp = [
    COMMENT "MOD begins",
    JZERO b 19,
    RESET tmp,
    ADD tmp b,
    SUB tmp a,
    JZERO tmp 2,
    JUMP 4,
    SHL b,
    INC c,
    JUMP (-7),
    RESET tmp,
    ADD tmp b,
    SUB tmp a,
    JZERO tmp 2,
    JUMP 2,
    SUB a b,
    SHR b,
    JZERO c 4,
    DEC c,
    JUMP (-9),
    RESET a]

-- res = a + b -- a,b untuched
addRegisters :: Register -> Register -> Register -> [Output]
addRegisters a b res = [RESET res, ADD res a, ADD res b]

-- a += b b untuched
addRegistersInPlace :: Register -> Register -> [Output]
addRegistersInPlace a b = [ADD a b]

-- res = a - b -- a,b untuched
subtractRegisters :: Register -> Register -> Register -> [Output]
subtractRegisters a b res = [RESET res, ADD res a, SUB res b]

-- a -= b b untuched
subtractRegistersInPlace :: Register -> Register -> [Output]
subtractRegistersInPlace a b = [SUB a b]

-- Generate code making jump if condition a <comperator> b is True. Uses extra register to keep original data. Makes ajustemnts for its lenght
compareVarToVar :: Register -> Comparator -> Register -> Register -> Integer -> [Output]
compareVarToVar a Translator.LE b tmp jump
    | jump < 0 = [ADD tmp a, SUB tmp b, JZERO tmp (jump-2)]
    | otherwise = [ADD tmp a, SUB tmp b, JZERO tmp jump]
compareVarToVar a Translator.LT b tmp jump 
    | jump < 0 = [ADD tmp b, SUB tmp a, JZERO tmp 2, JUMP (jump-3)]
    | otherwise = [ADD tmp b, SUB tmp a, JZERO tmp 2, JUMP jump]
compareVarToVar a Translator.GE b tmp jump = compareVarToVar b Translator.LE a tmp jump
compareVarToVar a Translator.GT b tmp jump = compareVarToVar b Translator.LT a tmp jump
compareVarToVar a Translator.EQ b tmp jump
    | jump < 0 = [ADD tmp b, SUB tmp a, JZERO tmp 2, JUMP 4, ADD tmp a, SUB tmp b, JZERO tmp (jump-6)]
    | otherwise = [ADD tmp b, SUB tmp a, JZERO tmp 2, JUMP 4, ADD tmp a, SUB tmp b, JZERO tmp jump]
compareVarToVar a Translator.NE b tmp jump
    | jump < 0 = [ADD tmp b, SUB tmp a, JZERO tmp 2, JUMP (jump-4), ADD tmp a, SUB tmp b, JZERO tmp 2, JUMP (jump-7)]
    | otherwise = [ADD tmp b, SUB tmp a, JZERO tmp 2, JUMP (jump+4), ADD tmp a, SUB tmp b, JZERO tmp 2, JUMP jump]

singleRegisterTester val [] = val
singleRegisterTester val (SHR _:xs) = singleRegisterTester (val `div` 2) xs
singleRegisterTester val (SHL _:xs) = singleRegisterTester (val * 2) xs
singleRegisterTester val (INC _:xs) = singleRegisterTester (val + 1) xs
singleRegisterTester val (DEC _:xs) = singleRegisterTester (val - 1) xs
singleRegisterTester _ (RESET _:xs) = singleRegisterTester 0 xs



test :: Integer -> Maybe (Integer, Integer)
test 0 = Nothing
test n 
    | (singleRegisterTester 1999 (generateConstFromConst 1999 n A)) ==  n = test (n-1)
    | otherwise = Just ((singleRegisterTester 1999 (generateConstFromConst 1999 n A)), n)
