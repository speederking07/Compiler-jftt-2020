{-# LANGUAGE ScopedTypeVariables #-}
import Lexer
import System.Environment (getProgName, getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode( .. ), hPutStr, hClose)
import Control.Exception (handle, IOException)
import System.Process

loadFile :: [Char] -> IO (Either [Char] String)
loadFile path = do 
    handle (\(e :: IOException) -> return $ Left $ "File "++path++" not found") $ do
    h <- openFile path ReadMode
    c <- hGetContents h
    return (Right c)

findTest :: String -> [([Integer], [Integer])]
findTest = go [] . lexer . nextLineSeparator
    where
        go acc [] = acc
        go acc (DATA_BEGIN:xs) = test ([], []) xs
        go acc (_:xs) = go acc xs
        test _ [] = []
        test (input, output) (NUM _: DOT: xs)
            | null output && null input = test ([], []) xs
            | otherwise = (input, output):test ([], []) xs
        test (input, output) (INPUT: NUM int:xs) = test (input++[int], output) xs
        test (input, output) (OUTPUT: NUM int:xs) = test (input, output++[int]) xs
        test (input, output) _ = [(input, output)]
        nextLineSeparator [] = []
        nextLineSeparator ('\\':'\n':xs) = ignoreWhite xs
        nextLineSeparator (x:xs) = x:nextLineSeparator xs
        ignoreWhite (x:xs)
            | x == ' ' || x == '\t' || x == '\r' || x == '\n' = ignoreWhite xs
            | otherwise = nextLineSeparator (x:xs)

findCost :: String -> [Integer] -> Maybe Integer
findCost input exp
    | isCorrect exp parsed = findCost parsed
    | otherwise = Nothing 
    where
        parsed = lexer input
        findCost [] = Nothing 
        findCost (COST:NUM cost:_) = Just cost
        findCost (_:xs) = findCost xs
        isCorrect [] _ = True 
        isCorrect (expected:rest) (OUTPUT:NUM result:xs)
            | expected == result = isCorrect rest xs
            | otherwise = False
        isCorrect exp (_:xs) = isCorrect exp xs

testProgram :: String -> String -> [Integer] -> [Integer] -> IO (Maybe Integer)
testProgram mv prog input exp = do    
    res <- readProcess mv [prog] (strinify input)
    return $ findCost res exp
    where
        strinify :: [Integer] -> String
        strinify input = concatMap ((++[' ']) . show) input

runListOfTests :: String -> String -> [String] -> IO()
runListOfTests compilator mv list = summary $ mapM test_program list
    where
        t = mapM test_program list
        test_program to_test = do
            readProcess compilator [to_test, "test.mr"] []            
            f <- loadFile to_test
            case f of
                Left e -> putStrLn e
                _ -> return ()
            let Right file = f            
            let tests = findTest file
            res <- sequence (runTests tests)
            let res_sum = sum <$> sequence res
            case res_sum of
                Nothing -> putStrLn $ "Test: "++to_test++"  ERROR !!!"
                Just x -> putStrLn $ "Test: "++to_test++" "++show x
            return res_sum
        summary :: IO [Maybe Integer] -> IO ()
        summary list = do
            l <- list
            let wrong = length $ filter isNothing l
            let all = length l
            let res = sum <$> sequence l
            case res of
                Nothing -> putStrLn $ "\nPassed "++show (all-wrong)++"/"++show all++" tests"
                Just x -> putStrLn $ "\nAll tests passed :) \nSummary cost: "++show x
            return ()
        isNothing Nothing = True 
        isNothing _ = False
        runTests :: [([Integer], [Integer])] -> [IO(Maybe Integer)]  
        runTests = fmap (uncurry (testProgram mv "test.mr"))

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName 
    if length args == 3 then run (head args) (args!!1) (args!!2)
    else putStrLn $ "Try "++progName++" ./kompilator ./mv ./list"
    

run :: String -> String -> [Char] -> IO ()
run compilator mv list = do
    Right list_input <- loadFile list
    let list_of_tests = lines list_input
    runListOfTests compilator mv list_of_tests
    return ()