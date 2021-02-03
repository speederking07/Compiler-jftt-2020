{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Memory
import BigConstSubstitution
import ArrayOptimalization
import Assigments
import ConstantsFolding
import Parser
import Lexer
import Compilator
import Correctnes
import Control.Monad.Trans.Except
import Translator
import Debug.Trace
import System.Environment (getProgName, getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode( .. ), hPutStr, hClose)
import Control.Exception (handle, IOException)

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName 
    if length args == 2 then run (head args) (args!!1)
    else putStrLn $ "Try "++progName++" <input_file> <output_file>"

run :: [Char] -> [Char] -> IO ()
run input_file output_file = do
    input <- loadFile input_file
    let code = input >>= parse . lexer . noWhite
    let program = do
        (dec, code) <- code
        (correct_dec, correct_code) <- checkCorrectness dec code
        let (for_dec, code') = pullOutForDeclarations correct_code 
        let code'' =  (removeUnnecessaryAssignments . foldConstants) code'
        let (array_dec, array_code) = optimizeArrays (correct_dec++for_dec) code''
        let (const_dec, const_code) = addConstToMemory array_code 
        let memory = getMemoryAllocation $ sortByUsefullnes const_code (array_dec++const_dec)
        return $ compile memory const_code
    case program of 
        Left error -> putStrLn error
        Right output -> do
            r <- saveFile output_file (prettyShow output)
            case r of
                Left error -> putStrLn "Critical error:" >> putStrLn error
                Right () -> putStrLn "Success"    
    where
        loadFile path = do 
            handle (\(e :: IOException) -> return $ Left $ "File "++path++" not found") $ do
            h <- openFile path ReadMode
            c <- hGetContents h
            return (Right c)
        saveFile path content = do
            handle (\(e :: IOException) -> return $ Left $ "File "++path++" could not be opend") $ do
            h <- openFile path WriteMode 
            hPutStr h content
            hClose h
            return $ Right ()
        noWhite [] = []
        noWhite ('\n':xs) = noWhite xs
        noWhite (' ':xs) = noWhite xs
        noWhite ('\t':xs) = noWhite xs
        noWhite (x:xs) = x:noWhite xs

