module Problems.Y2023.Problem9 where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
  input <- parseFile parseInput fp
  let diffs = map genDiffs input
  let diffs' = map (takeWhile (any (/= 0))) diffs
  let diffs'' = map addLast diffs'
  let ran = map runDiffs' diffs''
  return $ show $ sum $ map extract ran

runHard :: FilePath -> IO String
runHard fp = do
  input <- parseFile parseInput fp
  let diffs = map genDiffs input
  let diffs' = map (takeWhile (any (/= 0))) diffs
  let diffs'' = map addLast diffs'
  let ran = map runDiffs'' diffs''
  return $ show $ sum $ map extract ran

extract :: [[Int]] -> Int
extract xs = head $ head $ reverse xs

runDiffs' :: [[Int]] -> [[Int]]
runDiffs' ds = let diffs = map reverse ds
                   h = head diffs
                   seed = (0:h):(tail diffs)
               in runDiffs seed

runDiffs'' :: [[Int]] -> [[Int]]
runDiffs'' ds = let h = head ds 
                    seed = (0:h):(tail ds)
                in runHardDiffs seed

runHardDiffs :: [[Int]] -> [[Int]]
runHardDiffs [] = []
runHardDiffs (x:[]) = [x]
runHardDiffs (x:y:xs) = x : runHardDiffs (newY:xs)
                    where newY = runHardDiff x y

runHardDiff :: [Int] -> [Int] -> [Int]
runHardDiff xs ys = y:ys
                where hx = head xs
                      hy = head ys
                      y = hy - hx

runDiffs :: [[Int]] -> [[Int]]
runDiffs [] = []
runDiffs (x:[]) = [x]
runDiffs (x:y:xs) = x : runDiffs (newY:xs)
                where newY = runDiff x y

runDiff :: [Int] -> [Int] -> [Int]
runDiff xs ys = y:ys
            where hx = head xs
                  hy = head ys
                  y = hx + hy

addLast :: [[Int]] -> [[Int]]
addLast diffs = let last = diff $ head $ reverse diffs
                in (last :) $ reverse diffs

genDiffs :: [Int] -> [[Int]]
genDiffs ds = ds : genDiffs (diff ds)

diff :: [Int] -> [Int]
diff [] = []
diff (x:[]) = []
diff (x:y:xs) = (y - x):(diff (y:xs))

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [[Int]]
parseInput = sepEndBy1 (sepEndBy1 parseInt (char ' ')) eol