module Problems2024.Problem22 (runEasy, runHard) where

import Utils
import Data.Bits
import qualified Data.Map as M

runEasy :: FilePath -> IO String
runEasy fp = do
    seeds <- parseFast ((map read) . lines) fp
    return $ show $ sum $ map (last . take (2001) . secretSequence) seeds

runHard :: FilePath -> IO String
runHard fp = do
    seeds <- parseFast ((map read) . lines) fp
    let ds = map (buildDistribution . genDeltas . map (\n -> n `mod` 10) . take 2001 . secretSequence) seeds
        targetWiseSums = foldl (M.unionWith (+)) M.empty ds
    return $ show $ maximum $ M.elems targetWiseSums

genDeltas :: [Int] -> [(Int, Int)]
genDeltas s = zip (tail s) (deltas s)

secretSequence :: Int -> [Int]
secretSequence seed = scanl (\acc _ -> genSecret acc) seed [0..]

genSecret :: Int -> Int
genSecret = step3 . step2 . step1
    where step1 a = prune . (mix a) . x64 $ a
          step2 b = prune . (mix b) . div32 $ b
          step3 c = prune . (mix c) . x2048 $ c
          x64 = (*64)
          x2048 = (*2048)
          div32 x = x `div` 32
          mix n x = x `xor` n 
          prune x = x `mod` 16777216

type Target = (Int, Int, Int, Int)

buildDistribution :: [(Int,Int)] -> M.Map Target Int
buildDistribution xs | length xs < 4 = M.empty
                     | otherwise = M.insertWith const (a,b,c,d) k $ buildDistribution (tail xs)
    where k = fst $ xs !! 3
          (a:b:c:d:[]) = map snd $ take 4 xs

deltas :: [Int] -> [Int]
deltas [] = []
deltas (x:[]) = []
deltas (x:y:xs) = (y - x):(deltas (y:xs))