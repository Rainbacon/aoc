module Problems2025.Problem2 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

runEasy :: FilePath -> IO String
runEasy fp = do
    ranges <- parseFile (sepBy1 parseRange (char ',')) fp
    return $ show $ sum $ filter isInvalid $ concatMap expand ranges

expand :: (Int, Int) -> [Int]
expand (s, e) = [s..e]

isInvalid :: Int -> Bool
isInvalid i = evenLength && firstHalf == secondHalf
    where evenLength = length (show i) `mod` 2 == 0
          firstHalf = take half $ show i
          secondHalf = drop half $ show i
          half = length (show i) `div` 2

runHard :: FilePath -> IO String
runHard fp = do
    ranges <- parseFile (sepBy1 parseRange (char ',')) fp
    return $ show $ sum $ filter isInvalid' $ concatMap expand ranges

isInvalid' :: Int -> Bool
isInvalid' i = any replication chunked
    where chunked = map (chunkify (show i)) (factors (length $ show i))

replication :: [String] -> Bool
replication [] = True
replication (x:xs) = all (==x) xs

chunkify :: String -> Int -> [String]
chunkify s n | length s == 0 = []
             | otherwise = (take n s):(chunkify (drop n s) n)

factors :: Int -> [Int]
factors i = filter (\x -> i `mod` x == 0) [1..i `div` 2]

parseRange :: (Monad m) => ParsecT Void String m (Int, Int)
parseRange = do
    start <- parsePositive
    char '-'
    end <- parsePositive
    return (start, end)