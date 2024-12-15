module Problems.Y2024.Problem7 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

runEasy :: FilePath -> IO String
runEasy fp = do
    equations <- parseFile parseInput fp
    return $ show $ sum $ map fst $ filter (canSolve [(+), (*)]) equations

runHard :: FilePath -> IO String
runHard fp = do
    equations <- parseFile parseInput fp
    return $ show $ sum $ map fst $ filter (canSolve [(+), (*), concatInts]) equations

concatInts :: Int -> Int -> Int
concatInts a b = let aStr = show a
                     bStr = show b
                 in read (bStr ++ aStr)

canSolve :: [(Int -> Int -> Int)] -> (Int, [Int]) -> Bool
canSolve fns (target, vals) = target `elem` solutions
                          where solutions = genSolution $ reverse vals
                                genSolution [] = []
                                genSolution (x:[]) = [x]
                                genSolution (x:xs) = let rolling = genSolution xs
                                                     in map (\f -> f x) fns <*> rolling

parseInput :: (Monad m) => ParsecT Void String m [(Int, [Int])]
parseInput = sepEndBy1 parseEqn eol

parseEqn :: (Monad m) => ParsecT Void String m (Int, [Int])
parseEqn = do
    res <- parseInt
    char ':'
    some (char ' ')
    inputs <- sepBy1 parseInt (some (char ' '))
    return (res, inputs)