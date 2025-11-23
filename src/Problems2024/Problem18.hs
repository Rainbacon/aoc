module Problems2024.Problem18 (runEasy, runHard) where

import Utils.Parsing
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad.State as ST

runEasy :: FilePath -> IO String
runEasy fp = do
    bytes <- parseFile parseInput fp
    let gridSize = 70
        time = 1024
        grid = M.fromList $ zip [(x, y) | x <- [0..gridSize], y <- [0..gridSize]] (repeat 0)
        walls = foldl (\acc k -> M.adjust (+1) k acc) grid (take time bytes)
        target = (gridSize, gridSize)
    return $ show $ ST.evalState (aStar target walls genNeighbors) ([((0,0), 0)], S.empty)

genNeighbors :: Point -> Grid -> [(Point, Int)]
genNeighbors (x, y) grid = filter (\(p, _) -> M.member p grid && grid M.! p == 0) [((x - 1, y), 1), ((x + 1, y), 1), ((x, y - 1), 1), ((x, y + 1), 1)]

printGrid :: Grid -> String
printGrid grid = let str = M.map (\a -> if a == 1 then '#' else '.') grid
                 in unlines $ map (map snd) $ groupBy (\((a, _), _) ((b, _), _) -> a == b) $ M.assocs str

runHard :: FilePath -> IO String
runHard fp = do
    bytes <- parseFile parseInput fp
    let gridSize = 70
        grid = M.fromList $ zip [(x, y) | x <- [0..gridSize], y <- [0..gridSize]] (repeat 0)
        target = (gridSize, gridSize)
    return $ show $ ST.evalState (runSimulations bytes target) grid

runSimulations :: [Point] -> Point -> ST.State Grid (Maybe Point)
runSimulations [] _ = return Nothing
runSimulations (p:ps) target = do
    grid <- ST.get
    let walls = M.adjust (+1) p grid
    ST.put walls
    let res = ST.evalState (aStar target walls genNeighbors) ([((0,0), 0)], S.empty)
    if Y.isNothing res
    then return $ Just p
    else runSimulations ps target

parseInput :: (Monad m) => ParsecT Void String m [Point]
parseInput = sepEndBy1 parsePair eol

parsePair :: (Monad m) => ParsecT Void String m Point
parsePair = do
    a <- parseInt
    _ <- char ','
    b <- parseInt
    return (a, b)