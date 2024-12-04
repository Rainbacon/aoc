module Problems2024.Problem4 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast (M.fromList . mapPos . lines) fp
    let xs = M.filter (== 'X') input
    return $ show $ sum $ map (\x -> search x input) (M.keys xs)

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast (M.fromList . mapPos . lines) fp
    let as = M.filter (== 'A') input
    return $ show $ length $ filter id $ map (\a -> search' a input) (M.keys as)


search :: Point -> M.Map Point Char -> Int
search p m = let searchLines = generateLines p
                 searches = map (\l -> lookupLine l m) searchLines 
             in length $ filter (== "XMAS") searches

generateLines :: Point -> [[Point]]
generateLines (x, y) = [
    [(x, y), (x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3)],
    [(x, y), (x - 1, y), (x - 2, y), (x - 3, y)],
    [(x, y), (x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3)],
    [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)],
    [(x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3)],
    [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)],
    [(x, y), (x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3)],
    [(x, y), (x, y - 1), (x, y - 2), (x, y - 3)]]

lookupLine :: [Point] -> M.Map Point Char -> String
lookupLine ps m = map (\p -> resolve p m) ps
              where resolve point mapping = case M.lookup point mapping of
                                                (Just c) -> c
                                                Nothing -> ' '

search' :: Point -> M.Map Point Char -> Bool
search' p m = let searchLines = generateLines' p
                  searches = map (\l -> lookupLine l m) searchLines 
              in (==2) $ length $ filter (== "MAS") searches
                  
generateLines' :: Point -> [[Point]]
generateLines' (x, y) = [
    [(x - 1, y - 1), (x, y), (x + 1, y + 1)],
    [(x + 1, y - 1), (x, y), (x - 1, y + 1)],
    [(x + 1, y + 1), (x, y), (x - 1, y - 1)],
    [(x - 1, y + 1), (x, y), (x + 1, y - 1)]]