module Problems2015.Problem20 (runEasy, runHard) where

import Utils.Parsing

runEasy :: FilePath -> IO String
runEasy fp = do
    target <- parseFile parseInt fp
    return $ show $ head $ dropWhile ((< target) . snd) $ map calcPresents [1..]

calcPresents :: Int -> (Int, Int)
calcPresents house = (house, sum $ map (10*) $ factorize house)

factorize :: Int -> [Int]
factorize i = (i:) $ filter (\x -> i `mod` x == 0) [1..i `div` 2]

runHard :: FilePath -> IO String
runHard _ = return ""