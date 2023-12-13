module Problems2023.Problem12 where

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils
import Debug.Trace

data Spring = Op | Dam | Unkn
    deriving (Eq)

instance Show Spring where
    show Op = "."
    show Dam = "#"
    show Unkn = "?"

isDam :: Spring -> Bool
isDam = (==) Dam

isOp :: Spring -> Bool
isOp = (==) Op

isUnkn :: Spring -> Bool
isUnkn = (==) Unkn

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let allPerms = map (fmap genPerms) input
    return $ show $ sum $ map countValid allPerms

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let expanded = map expand input
    let solved = map solveMemo expanded
    return $ show $ sum $ solved

expand :: ([Int], [Spring]) -> ([Int], [Spring])
expand (nums, springs) = (concat $ replicate 5 nums, concatWith Unkn $ replicate 5 springs)

concatWith :: a -> [[a]] -> [a]
concatWith _ [] = []
concatWith _ (x:[]) = x
concatWith a (x:xs) = x ++ [a] ++ concatWith a xs 

solveMemo :: ([Int], [Spring]) -> Int
solveMemo (nums, springs) = solve nums springs where 
    ln = length nums
    ls = length springs
    memo = A.array ((0,0), (ln, ls)) [((x, y), solve' (drop x nums) (drop y springs)) | x <- [0..ln], y <- [0..ls]]

    solve :: [Int] -> [Spring] -> Int
    solve ns ss = memo A.! (ln - length ns, ls - length ss)

    solve' :: [Int] -> [Spring] -> Int
    solve' [] [] = 1
    solve' _ [] = 0
    solve' [] xs | Dam `elem` xs = 0 
                 | otherwise = 1
    solve' ys (Op:xs) = solve ys xs
    solve' (y:ys) xs@(Dam:_) | length xs < y = 0
                             | length notOps < y = 0
                             | length xs == y = solve ys (drop (y + 1) xs)
                             | isDam following = 0
                             | otherwise = solve ys (drop (y + 1) xs)
                           where notOps = takeWhile (not . isOp) xs
                                 following = head $ drop y xs
    solve' ys (Unkn:xs) = solve ys xs + solve' ys (Dam:xs)

genPerms :: [Spring] -> [[Spring]]
genPerms [] = []
genPerms (Unkn:xs) | rest == [] = [[Op], [Dam]]
                   | otherwise = map (Op:) rest ++ map (Dam:) rest
                 where rest = genPerms xs
genPerms (x:xs) | rest == [] = [[x]]
                | otherwise = map (x:) $ rest
              where rest = genPerms xs

countValid :: ([Int], [[Spring]]) -> Int
countValid (nums, perms) = length $ filter id $ map (\p -> isValid (nums, p)) perms

isValid :: ([Int], [Spring]) -> Bool
isValid (nums, springs) = length springs' == length nums && all id (zipWith (==) springs' nums)
                      where springs' = map length $ filter (\xs -> isDam $ head xs) $ concatSame springs

concatSame :: Eq a => [a] -> [[a]]
concatSame [] = []
concatSame xs@(x:_) = let h = takeWhile (== x) xs
                          r = dropWhile (== x) xs
                      in h:(concatSame r)

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [([Int], [Spring])]
parseInput = sepEndBy1 parseRow eol

parseRow :: (Monad m) => ParsecT Void String m ([Int], [Spring])
parseRow = do
    springs <- some parseSpring
    char ' '
    nums <- sepEndBy1 parseInt (char ',')
    return (nums, springs)

parseSpring :: (Monad m) => ParsecT Void String m Spring
parseSpring = (char '.' *> pure Op) <|> (char '#' *> pure Dam) <|> (char '?' *> pure Unkn)