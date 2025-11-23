module Problems2023.Problem1 (runEasy, runHard) where

import Utils.Parsing
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- readFile fp
    return $ show $ sum $ map toInt (lines input)

runHard :: FilePath -> IO String
runHard fp = do
    input <- readFile fp
    let x = map toInt' (lines input)
    return $ show $ sum $ x

toInt :: String -> Int 
toInt s = let digits = filter isDigit s
              first = head digits
              last = head $ reverse digits
           in read (first:last:[])

toInt' :: String -> Int
toInt' s = let maybes = map matchDigit $ tails s
               digits = map (fromJust) $ filter isJust maybes
               first = head digits
               last = head $ reverse digits
            in 10 * first + last

matchDigit :: String -> Maybe Int
matchDigit ('z':'e':'r':'o':_) = return 0
matchDigit ('o':'n':'e':_) = return 1
matchDigit ('t':'w':'o':_) = return 2
matchDigit ('t':'h':'r':'e':'e':_) = return 3
matchDigit ('f':'o':'u':'r':_) = return 4
matchDigit ('f':'i':'v':'e':_) = return 5
matchDigit ('s':'i':'x':_) = return 6
matchDigit ('s':'e':'v':'e':'n':_) = return 7
matchDigit ('e':'i':'g':'h':'t':_) = return 8
matchDigit ('n':'i':'n':'e':_) = return 9
matchDigit (x:_) | isDigit x = return (read [x]) 
                 | otherwise = Nothing
matchDigit [] = Nothing