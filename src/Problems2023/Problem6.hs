module Problems2023.Problem6  where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

runEasy :: FilePath -> IO String
runEasy fp = do
  input <- parseFile parseInput fp
  return $ show $ product $ map (length . calcDists) input

runHard :: FilePath -> IO String
runHard fp = do
  input <- parseFile parseInput' fp
  return $ show $ length $ calcDists input

calcDist :: Int -> Int  -> Int
calcDist t n = n * (t - n)

calcDists :: (Int, Int) -> [Int]
calcDists (t, l) = let dists = map (calcDist t) [0..t]
                   in filter (> l) dists

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [(Int, Int)]
parseInput = do
  some letterChar *> char ':' *> some spaceChar
  times <- sepEndBy1 parseInt (some spaceChar)
  some letterChar *> char ':' *> some spaceChar
  distances <- sepEndBy1 parseInt (some spaceChar)
  return $ zip times distances

parseInput' :: (Monad m) => ParsecT Void String m (Int, Int)
parseInput' = do
  some letterChar *> char ':' *> some spaceChar
  times <- sepEndBy1 (some digitChar) (some spaceChar)
  some letterChar *> char ':' *> some spaceChar
  distances <- sepEndBy1 (some digitChar) (some spaceChar)
  return (read $ concat times, read $ concat distances)