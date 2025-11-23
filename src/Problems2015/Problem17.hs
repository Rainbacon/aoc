module Problems2015.Problem17 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
    containers <- parseFile (sepEndBy1 parseInt eol) fp
    return $ show $ determineWays 150 containers

determineWays :: Int -> [Int] -> Int
determineWays target [] = 0
determineWays target (x:xs) | target == x = 1 + determineWays target xs
                            | target < x = determineWays target xs
                            | otherwise = determineWays (target - x) xs + determineWays target xs

runHard :: FilePath -> IO String
runHard fp = do
    containers <- parseFile (sepEndBy1 parseInt eol) fp
    let perms = determineWays' 150 containers
        m = minimum $ map length perms
    return $ show $ length $ filter (==m) $ map length perms

determineWays' :: Int -> [Int] -> [[Int]]
determineWays' target [] = []
determineWays' target (x:xs) | target == x = [[x]] ++ determineWays' target xs
                             | target < x = determineWays' target xs
                             | otherwise = map (x:) (determineWays' (target - x) xs) ++ (determineWays' target xs)
