module Problems2022.Problem8 where

import qualified Data.Char as C
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let rows = length input - 1
    let cols = length (head input) - 1
    let coords = [visible x y input | x <- [0..rows], y <- [0..cols]]
    return $ show $ length $ filter id coords

runHard :: FilePath -> IO String
runHard fp = do 
    input <- parseFile parseInput fp
    let rows = length input - 1
    let cols = length (head input) - 1
    let scores = [sceneScore x y input | x <- [0..rows], y <- [0..cols]]
    return $ show $ maximum scores

parseInput :: (Monad m) => ParsecT Void String m [[Int]] 
parseInput = sepEndBy1 parseRow eol
        where parseRow = ((map C.digitToInt) <$> some digitChar)

visible :: Int -> Int -> [[Int]] -> Bool
visible 0 _ _ = True
visible _ 0 _ = True
visible x y trees = let row = trees !! x
                        col = map (!! y) trees
                        tree = head $ drop y row
                        los = [take y row, drop (y + 1) row, take x col, drop (x + 1) col]
                    in foldr (||) False $ map (all (< tree)) los

sceneScore :: Int -> Int -> [[Int]] -> Int
sceneScore x y trees = let row = trees !! x
                           col = map (!! y) trees
                           tree = head $ drop y row
                           los = [reverse $ take y row, drop (y + 1) row, reverse $ take x col, drop (x + 1) col]
                       in product $ map (length . (takeWhile' tree)) los

takeWhile' :: (Ord a) => a -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' test (x:xs) | x < test = x:(takeWhile' test xs)
                       | otherwise = [x]
