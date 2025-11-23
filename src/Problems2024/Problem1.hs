module Problems2024.Problem1 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let first = sort $ fst input
    let second = sort $ snd input
    return $ show $ sum $ zipWith (\f s -> abs (f - s)) first second

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    return $ show $ sum $ map (calcSimilarity (snd input)) $ fst input

calcSimilarity :: [Int] -> Int -> Int
calcSimilarity xs n = let filtered = filter (== n) xs
                    in n * length filtered

parseInput :: (Monad m) => ParsecT Void String m ([Int], [Int]) 
parseInput = do
    inputLines <- sepEndBy1 parseLine eol
    return (map fst inputLines, map snd inputLines)

parseLine :: (Monad m) => ParsecT Void String m (Int, Int)
parseLine = do
    num1 <- parseInt
    some (char ' ')
    num2 <- parseInt
    return (num1, num2)
