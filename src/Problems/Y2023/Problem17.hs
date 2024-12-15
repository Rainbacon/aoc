module Problems.Y2023.Problem17 (runEasy, runHard) where

import qualified Data.Array as A
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Char
import Data.List
import Utils.Parsing
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let target = findTarget input
    let initialState = ([((1, 0), Y.fromJust $ M.lookup (1, 0) input, S, 1), ((0, 1), Y.fromJust $ M.lookup (0, 1) input, E, 1)], S.empty)
    return $ show $ ST.evalState (aStar' target input generateNeighbors) initialState

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let target = findTarget input
    let initialState = ([((1, 0), Y.fromJust $ M.lookup (1, 0) input, S, 1), ((0, 1), Y.fromJust $ M.lookup (0, 1) input, E, 1)], S.empty)
    return $ show $ ST.evalState (aStar' target input generateNeighbors') initialState

findTarget :: Grid -> Point
findTarget grid = maximum $ M.keys grid

generateNeighbors :: Point -> Grid -> CompassDirection -> Int -> [(Point, Int, CompassDirection, Int)]
generateNeighbors p@(x, y) grid dir mag | mag >= 3 = map (\(p1, d, m) -> (p1, Y.fromJust $ M.lookup p1 grid, d, m)) $ filter (\(p2,_,_) -> inBounds' grid p2) [left, right]
                                        | otherwise = map (\(p1, d, m) -> (p1, Y.fromJust $ M.lookup p1 grid, d, m)) $ filter (\(p2,_,_) -> inBounds' grid p2) [left, straight, right]
                                      where left | dir == E = ((x - 1, y), N, 1)
                                                 | dir == W = ((x + 1, y), S, 1)
                                                 | dir == N = ((x, y - 1), W, 1)
                                                 | otherwise = ((x, y + 1), E, 1)
                                            right | dir == E = ((x + 1, y), S, 1)
                                                  | dir == W = ((x - 1, y), N, 1)
                                                  | dir == N = ((x, y + 1), E, 1)
                                                  | otherwise = ((x, y - 1), W, 1)
                                            straight | dir == E = ((x, y + 1), dir, mag + 1) 
                                                     | dir == W = ((x, y - 1), dir, mag + 1)
                                                     | dir == S = ((x + 1, y), dir, mag + 1)
                                                     | otherwise = ((x - 1, y), dir, mag + 1)

generateNeighbors' :: Point -> Grid -> CompassDirection -> Int -> [(Point, Int, CompassDirection, Int)]                  
generateNeighbors' p@(x, y) grid dir mag | mag >= 10 = map (\(p1, d, m) -> (p1, Y.fromJust $ M.lookup p1 grid, d, m)) $ filter (\(p2,_,_) -> inBounds' grid p2) [left, right]
                                         | mag < 4 = map (\(p1, d, m) -> (p1, Y.fromJust $ M.lookup p1 grid, d, m)) $ filter (\(p2,_,_) -> inBounds' grid p2) [straight]
                                         | otherwise = map (\(p1, d, m) -> (p1, Y.fromJust $ M.lookup p1 grid, d, m)) $ filter (\(p2,_,_) -> inBounds' grid p2) [left, straight, right]
                                       where left | dir == E = ((x - 1, y), N, 1)
                                                  | dir == W = ((x + 1, y), S, 1)
                                                  | dir == N = ((x, y - 1), W, 1)
                                                  | otherwise = ((x, y + 1), E, 1)
                                             right | dir == E = ((x + 1, y), S, 1)
                                                   | dir == W = ((x - 1, y), N, 1)
                                                   | dir == N = ((x, y + 1), E, 1)
                                                   | otherwise = ((x, y - 1), W, 1)
                                             straight | dir == E = ((x, y + 1), dir, mag + 1) 
                                                      | dir == W = ((x, y - 1), dir, mag + 1)
                                                      | dir == S = ((x + 1, y), dir, mag + 1)
                                                      | otherwise = ((x - 1, y), dir, mag + 1)

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m Grid
parseInput = do
    ls <- sepEndBy1 parseLine eol
    return $ M.fromList $ mapPos ls

parseLine :: (Monad m) => ParsecT Void String m [Int]
parseLine = map digitToInt <$> some digitChar