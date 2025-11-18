module Problems2015.Problem9 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import Data.List

type Distances = M.Map String (M.Map String Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    distances <- parseFile parseInput fp
    return $ show $ findShortest distances

findShortest :: Distances -> Int
findShortest ds = head $ sort $ distances
    where allCities = M.keys ds
          possibleRoutes = permutations allCities
          distances = map (findDistance ds) possibleRoutes

findDistance :: Distances -> [String] -> Int
findDistance ds [] = 0
findDistance ds (x:[]) = 0
findDistance ds (x:y:xs) = d + findDistance ds (y:xs)
    where d = (ds M.! x) M.! y


runHard :: FilePath -> IO String
runHard fp = do
    distances <- parseFile parseInput fp
    return $ show $ findLongest distances

findLongest :: Distances -> Int
findLongest ds = head $ reverse $ sort $ distances
    where allCities = M.keys ds
          possibleRoutes = permutations allCities
          distances = map (findDistance ds) possibleRoutes

parseInput :: (Monad m) => ParsecT Void String m Distances
parseInput = do
    distances <- sepEndBy1 parseDistance eol
    return $ foldl1 (M.unionWith M.union) distances

parseDistance :: (Monad m) => ParsecT Void String m Distances
parseDistance = do
    c1 <- some letterChar
    string " to "
    c2 <- some letterChar
    string " = "
    d <- parseInt
    return $ M.fromList [(c1, M.singleton c2 d), (c2, M.singleton c1 d)]
