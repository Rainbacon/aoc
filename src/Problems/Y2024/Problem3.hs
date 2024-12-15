module Problems.Y2024.Problem3 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Maybe as Y

type InputData = [(Int, Int)] 

data Instruction = Do | Dont | Mul Int Int
    deriving Show

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ sum $ map mul input

mul :: (Int, Int) -> Int
mul (a, b) = a * b

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInputHard fp
    return $ show $ processHard True input

processHard :: Bool -> [Instruction] -> Int
processHard _ [] = 0
processHard False (Do:xs) = processHard True xs
processHard False (Dont:xs) = processHard False xs
processHard False (x:xs) = processHard False xs
processHard True (Dont:xs) = processHard False xs
processHard True (Do:xs) = processHard True xs
processHard True ((Mul a b):xs) = (a * b) + processHard True xs

-------- Parsing ---------
parseInput :: (Monad m) => ParsecT Void String m InputData
parseInput = do
    input <- many parseThing
    return $ map Y.fromJust $ filter Y.isJust input

parseThing :: (Monad m) => ParsecT Void String m (Maybe (Int, Int))
parseThing = (try parseMul') <|> parseGarbage       

parseGarbage :: (Monad m) => ParsecT Void String m (Maybe a)
parseGarbage = do
    anySingle
    return Nothing

parseMul' :: (Monad m) => ParsecT Void String m (Maybe (Int, Int))
parseMul' = do
    lookAhead parseMul
    val <- parseMul
    return val

parseMul :: (Monad m) => ParsecT Void String m (Maybe (Int, Int))
parseMul = do
    string "mul("
    num1 <- parseInt
    char ','
    num2 <- parseInt
    char ')'
    return $ Just (num1, num2)


parseInputHard :: (Monad m) => ParsecT Void String m [Instruction]
parseInputHard = do
    input <- many parseThingHard
    return $ map Y.fromJust $ filter Y.isJust input

parseThingHard :: (Monad m) => ParsecT Void String m (Maybe Instruction)
parseThingHard = (try parseMul'') <|> (try parseDo) <|> (try parseDont) <|> parseGarbage

parseDo :: (Monad m) => ParsecT Void String m (Maybe Instruction)
parseDo = do
    lookAhead (string "do()")
    string "do()"
    return $ Just Do

parseDont :: (Monad m) => ParsecT Void String m (Maybe Instruction)
parseDont = do
    lookAhead (string "don't()")
    string "don't()"
    return $ Just Dont

parseMul'' :: (Monad m) => ParsecT Void String m (Maybe Instruction)
parseMul'' = do
    lookAhead parseMul
    val <- parseMul
    let (a, b) = Y.fromJust val
    return $ Just (Mul a b)