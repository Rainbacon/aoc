module Problems2015.Problem15 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Ingredient = Ingredient {
    name :: String
  , capacity :: Int
  , durability :: Int
  , flavor :: Int
  , texture :: Int
  , calories :: Int
} deriving (Show)

runEasy :: FilePath -> IO String
runEasy fp = do
    ingredients <- parseFile parseInput fp
    let possibleCookies = filter ((== 100) . sum) $ genCookies (length ingredients)
    return $ show $ maximum $ map (score ingredients) possibleCookies 

genCookies :: Int -> [[Int]]
genCookies 0 = [[]]
genCookies i = map (:) [0..100] <*> genCookies (i - 1)

score :: [Ingredient] -> [Int] -> Int
score ingredients cookie = product $ map flatten components
    where score' i (Ingredient _ c d f t _) = [i*c, i*d, i*f, i*t]
          components = foldl1 (zipWith (+)) $ zipWith score' cookie ingredients
          flatten i | i < 0 = 0
                    | otherwise = i


runHard :: FilePath -> IO String
runHard fp = do
    ingredients <- parseFile parseInput fp
    let possibleCookies = filter (cal500 ingredients) $ filter ((== 100) . sum) $ genCookies (length ingredients)
    return $ show $ maximum $ map (score ingredients) possibleCookies 

cal500 :: [Ingredient] -> [Int] -> Bool
cal500 ingredients cookie = 500 == (sum $ zipWith (\i n -> n * (calories i)) ingredients cookie)

parseInput :: (Monad m) => ParsecT Void String m [Ingredient]
parseInput = sepEndBy1 parseIngredient eol

parseIngredient :: (Monad m) => ParsecT Void String m Ingredient
parseIngredient = do
    n <- some letterChar
    string ": "
    attrs <- sepBy1 (some letterChar >> char ' ' >> parseInt) (string ", ")
    return $ Ingredient n (attrs !! 0) (attrs !! 1) (attrs !! 2) (attrs !! 3) (attrs !! 4)