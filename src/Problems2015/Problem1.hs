module Problems2015.Problem1 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ head $ reverse input

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    return $ show $ fst $ head $ dropWhile (\(_, a) -> a /= (-1)) $ zip [0..] input

parseInput :: (Monad m) => ParsecT Void String m [Int]
parseInput = do
    steps <- manyTill (up <|> down) eol
    return $ scanl (+) 0 steps

up :: (Monad m) => ParsecT Void String m Int
up = do
    char '(' 
    return $ 1

down :: (Monad m) => ParsecT Void String m Int
down = do
    char ')'
    return $ (-1)