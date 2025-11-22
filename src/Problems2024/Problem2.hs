module Problems2024.Problem2 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

type InputData = [[Int]]

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ length $ filter id $ map isSafe input

isSafe :: [Int] -> Bool
isSafe xs | sorted == xs = pairSafe xs
          | sorted == (reverse xs) = pairSafe xs
          | otherwise = False
        where sorted = sort xs
    
pairSafe :: [Int] -> Bool
pairSafe [] = True
pairSafe (x:[]) = True
pairSafe (x:y:xs) = let a = abs (x - y)
                        safe = a >= 1 && a <= 3
                    in safe && pairSafe (y:xs)


runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    return $ show $ length $ filter id $ map (isSafe' []) input

isSafe' :: [Int] -> [Int] -> Bool
isSafe' _ [] = True
isSafe' xs (y:[]) = isSafe xs
isSafe' xs (y:ys) | isSafe (xs ++ ys) = True
                  | otherwise = isSafe' (xs ++ [y]) ys



------- Parsing ---------
parseInput :: (Monad m) => ParsecT Void String m InputData
parseInput = sepEndBy1 parseReport eol



parseReport :: (Monad m) => ParsecT Void String m [Int]
parseReport = sepBy1 parseInt (char ' ')