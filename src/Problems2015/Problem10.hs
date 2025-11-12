module Problems2015.Problem10 (runEasy, runHard) where

import Utils

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast id fp
    return $ show $ length $ foldl (\a _ -> expand a) input [1..40]

expand :: String -> String
expand [] = []
expand all@(x:xs) = expanded' ++ expand xs'
    where expanded = takeWhile (==x) all
          xs' = dropWhile (==x) xs
          expanded' = (show $ length expanded) ++ (x:[])

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast id fp
    return $ show $ length $ foldl (\a _ -> expand a) input [1..50]
