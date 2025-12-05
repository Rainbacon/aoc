module Problems2025.Problem3 (runEasy, runHard) where

import Utils.Parsing
import Data.List
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast lines fp
    let start = "00"
    return $ show $ sum $ map (read . (foldl applyDigit start)) input

takeHighestTwo :: (Char, Char) -> Char -> (Char, Char)
takeHighestTwo (a, b) c = unjoin $ maximum [read [a,b], read [a,c], read [b,c]] 

unjoin :: Int -> (Char, Char)
unjoin i | length x == 1 = ('0', x !! 0)
         | otherwise = (x !! 0, x !! 1)
    where x = show i

conjoin :: (Char, Char) -> Int
conjoin (a, b) = read [a,b]

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast lines fp
    let start = replicate 12 '0'
    return $ show $ sum $ map (read . (foldl applyDigit start)) input

applyDigit :: String -> Char -> String
applyDigit number digit = maximum (number:substitutions)
    where digits = length number
          droppeds = zipWith remove [0..digits-1] (repeat number)
          substitutions = map (\s -> s ++ [digit]) droppeds

remove :: Int -> [a] -> [a]
remove i xs = front ++ back
    where front = take i xs
          back = drop (i + 1) xs