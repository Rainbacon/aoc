module Problems2015.Problem5 (runEasy, runHard) where

import Utils
import qualified Data.Map as M

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast lines fp
    return $ show $ length $ filter isNice input

hasVowels :: String -> Bool
hasVowels s = let vowels = filter isVowel s
              in length vowels > 2

isNice :: String -> Bool
isNice s = hasVowels s && correctSubs s

correctSubs :: String -> Bool
correctSubs s = hasDouble && noNaughty
    where doubles = map double "abcdefghijklmnopqrstuvwxyz"
          double c = c:c:[]
          hasDouble = length (filter (\s -> s `elem` doubles) subs) > 0
          noNaughty = length (filter (\s -> s `elem` ["ab", "cd", "pq", "xy"]) subs) == 0
          subs = substrings 2 s

substrings :: Int -> String -> [String]
substrings n s 
    | length s < n = []
    | otherwise = take n s : substrings n (tail s)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast lines fp
    return $ show $ length $ filter isNice' input

isNice' :: String -> Bool
isNice' s = nonOverlappingSubstrings s && uwu s

uwu :: String -> Bool
uwu s = length (filter uwu' trigrams) > 0
    where trigrams = substrings 3 s
          uwu' tg = (head tg) == (head $ reverse tg)

nonOverlappingSubstrings :: String -> Bool
nonOverlappingSubstrings s = M.size filtered > 0
    where digrams = substrings 2 s
          mapping = foldl folder M.empty $ zip [0..] digrams
          folder m (i, s) = M.insertWith (++) s [i] m
          filtered = M.filter nonOverlapping mapping
          nonOverlapping xs = length xs > 1 && (abs ((head $ reverse xs) - (head xs)) > 1)
