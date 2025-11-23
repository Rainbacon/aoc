module Problems2015.Problem4 (runEasy, runHard) where

import Utils.Parsing
import Data.Hash.MD5

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast id fp
    return $ show $ head $ dropUntil (checkHash input) [1..]

checkHash :: String -> Int -> Bool
checkHash key i = "00000" == (take 5 hash)
              where hash = md5s $ Str (key ++ (show i))
    
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f [] = []
dropUntil f a@(x:xs) | f x = a
                     | otherwise = dropUntil f xs

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast id fp
    return $ show $ head $ dropUntil (checkHash' input) [1..]

checkHash' :: String -> Int -> Bool
checkHash' key i = "000000" == (take 6 hash)
               where hash = md5s $ Str (key ++ (show i))