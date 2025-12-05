module Problems2025.Problem5 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type FreshIngredient = (Int, Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    (fresh, ingredients) <- parseFile parseIngredients fp
    return $ show $ length $ filter (isFresh fresh) ingredients

isFresh :: [FreshIngredient] -> Int -> Bool
isFresh xs i = any isFresh' xs
    where isFresh' (a, b) = a <= i && i <= b

runHard :: FilePath -> IO String
runHard fp = do
    (fresh, _) <- parseFile parseIngredients fp
    return $ show $ sum $ map size $ merge fresh

size :: FreshIngredient -> Int
size (a, b) = (b - a) + 1

merge :: [FreshIngredient] -> [FreshIngredient]
merge [] = []
merge (x:[]) = [x]
merge (x:xs) | anyOverlap = merge ((merge' x overlap):nonOverlap)
             | otherwise = x : merge xs
    where anyOverlap = length overlap > 0
          overlap = filter (hasOverlap x) xs
          nonOverlap = filter (not . (hasOverlap x)) xs
          merge' y ys = foldl mergeRanges y ys

hasOverlap :: FreshIngredient -> FreshIngredient -> Bool
hasOverlap (a, b) (c, d) | a <= c && b >= c = True
                         | a <= d && b >= d = True
                         | c <= a && d >= b = True
                         | otherwise = False
     
mergeRanges :: FreshIngredient -> FreshIngredient -> FreshIngredient
mergeRanges (a, b) (c, d) = (min a c, max b d)

parseIngredients :: (Monad m) => ParsecT Void String m ([FreshIngredient], [Int])
parseIngredients = do
    fresh <- sepEndBy1 parseRange eol
    eol
    ingredients <- sepEndBy1 parseInt eol
    return (fresh, ingredients)

parseRange :: (Monad m) => ParsecT Void String m (Int, Int)
parseRange = do
    start <- parsePositive
    char '-'
    end <- parsePositive
    return (start, end)