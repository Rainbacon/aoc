module Problems2024.Problem5 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Debug.Trace

type InputData = ([(Int, Int)], [[Int]]) 

runEasy :: FilePath -> IO String
runEasy fp = do
    (rules, updates) <- parseFile parseInput fp
    let ruleMap = buildRules rules
        flipped = map reverse updates
        inOrder = filter (isInOrder ruleMap) flipped
    return $ show $ sum $ map middle inOrder

runHard :: FilePath -> IO String
runHard fp = do
    (rules, updates) <- parseFile parseInput fp
    let ruleMap = buildRules rules
        flipped = map reverse updates
        notInOrder = filter (not . (isInOrder ruleMap)) flipped
    return $ show $ sum $ map (middle . (order ruleMap)) notInOrder

buildRules :: [(Int, Int)] -> M.Map Int [Int]
buildRules [] = M.empty
buildRules ((a,b):xs) = let rules = buildRules xs
                        in M.insertWith (++) a [b] rules

isInOrder :: M.Map Int [Int] -> [Int] -> Bool
isInOrder rules [] = True
isInOrder rules (x:xs) = let rule = M.lookup x rules
                             rest = isInOrder rules xs
                         in case rule of
                            (Just rs) -> isInOrder' xs rs && rest
                            Nothing -> rest

isInOrder' :: [Int] -> [Int] -> Bool
isInOrder' as bs = as \\ bs == as

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

order :: M.Map Int [Int] -> [Int] -> [Int]
order rules xs = sortBy (lookupRules rules) xs

lookupRules :: M.Map Int [Int] -> Int -> Int -> Ordering
lookupRules rules a b | noRules = EQ
                      | bInARule = GT
                      | aInBRule = LT
                      | otherwise = EQ
                    where aRule = M.lookup a rules
                          bRule = M.lookup b rules
                          noRules = Y.isNothing aRule && Y.isNothing bRule
                          bInARule = Y.isJust aRule && b `elem` (Y.fromJust aRule)
                          aInBRule = Y.isJust bRule && a `elem` (Y.fromJust bRule)

parseInput :: (Monad m) => ParsecT Void String m InputData
parseInput = do 
    rules <- sepEndBy1 parseRule eol
    eol
    updates <- sepEndBy1 parseUpdate eol
    return (rules, updates)

parseRule :: (Monad m) => ParsecT Void String m (Int, Int)
parseRule = do
    num1 <- parseInt
    char '|'
    num2 <- parseInt
    return (num1, num2)

parseUpdate :: (Monad m) => ParsecT Void String m [Int]
parseUpdate = sepBy1 parseInt (char ',')