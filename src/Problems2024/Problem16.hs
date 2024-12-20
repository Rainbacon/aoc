module Problems2024.Problem16 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import Control.Monad
import qualified Control.Monad.State as ST
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.List
import Debug.Trace

data Maze = Wall | Start | End | Empty
    deriving (Eq)
type QItem = (Point, Int, CompassDirection, S.Set Point) 
type PQ = [QItem]
type Seen = M.Map (Point, CompassDirection) Int
type AStarState = (PQ, Maybe Int, [(Int, S.Set Point)], Seen)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let target = head $ M.keys $ M.filter (== End) input
        start = head $ M.keys $ M.filter (== Start) input
    return $ show $ ST.evalState (aStar'' target input genNeighbors) ([(start, 0, E, S.empty)], Nothing, [], M.empty)

genNeighbors :: Point -> S.Set Point -> M.Map Point Maze -> CompassDirection -> PQ
genNeighbors p@(x, y) path grid E = filter (\(p, _, _, _) -> (grid M.! p) /= Wall) [((x, y + 1), 1, E, S.insert p path), ((x - 1, y), 1001, N, S.insert p path), ((x + 1, y), 1001, S, S.insert p path)]
genNeighbors p@(x, y) path grid N = filter (\(p, _, _, _) -> (grid M.! p) /= Wall) [((x - 1, y), 1, N, S.insert p path), ((x, y - 1), 1001, W, S.insert p path), ((x, y + 1), 1001, E, S.insert p path)]
genNeighbors p@(x, y) path grid W = filter (\(p, _, _, _) -> (grid M.! p) /= Wall) [((x, y - 1), 1, W, S.insert p path), ((x + 1, y), 1001, S, S.insert p path), ((x - 1, y), 1001, N, S.insert p path)]
genNeighbors p@(x, y) path grid S = filter (\(p, _, _, _) -> (grid M.! p) /= Wall) [((x + 1, y), 1, S, S.insert p path), ((x, y + 1), 1001, E, S.insert p path), ((x, y - 1), 1001, W, S.insert p path)]

aStar'' :: Point -> M.Map Point Maze -> (Point -> S.Set Point -> M.Map Point Maze-> CompassDirection -> PQ) -> ST.State AStarState Int 
aStar'' target grid constructNeighbors = do
    (queue, ans, paths, seen) <- ST.get
    let d@(p@(x, y), distance, dir, prev) = head queue
    let minPath = M.lookup (p, dir) seen
    if Y.isJust minPath && Y.fromJust minPath < distance
    then do
        ST.put (safeTail queue, ans, paths, seen)
        aStar'' target grid constructNeighbors
    else do
        if Y.isJust ans && Y.fromJust ans < distance
        then do
            return distance
        else do
            if p == target
            then do
                ST.put (safeTail queue, Just distance, (distance, S.insert p prev):paths, seen)
                aStar'' target grid constructNeighbors
            else do
                let newPaths = constructNeighbors p prev grid dir
                let newQueue = insertPaths' (map (\(a, i, b, c) -> (a, i + distance, b, c)) newPaths) $ safeTail queue
                ST.put (newQueue, ans, paths, M.insert (p, dir) distance seen)
                aStar'' target grid constructNeighbors

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

insertPaths' :: PQ -> PQ -> PQ
insertPaths' [] q = q
insertPaths' (x:xs) q = let rest = insertPaths' xs q
                        in insertPath' x rest

insertPath' :: QItem -> PQ -> PQ
insertPath' x [] = [x]
insertPath' a@(_, i, _, _) (b@(_, j, _, _):xs) | j >= i = (a:b:xs)
                                               | otherwise = b:(insertPath' a xs)

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let target = head $ M.keys $ M.filter (== End) input
        start = head $ M.keys $ M.filter (== Start) input
        (_, _, paths, _) = ST.execState (aStar'' target input genNeighbors) ([(start, 0, E, S.empty)], Nothing, [], M.empty)
    return $ show $ S.size $ foldl S.union S.empty $ map snd paths 

parseInput :: (Monad m) => ParsecT Void String m (M.Map Point Maze)
parseInput = do 
    grid <- sepEndBy1 parseGridRow eol
    return $ M.fromList $ mapPos grid

parseGridRow :: (Monad m) => ParsecT Void String m [Maze]
parseGridRow = many parseUnit

parseUnit :: (Monad m) => ParsecT Void String m Maze
parseUnit = (char '#' >> return Wall) <|> (char '.' >> return Empty) <|> (char 'S' >> return Start) <|> (char 'E' >> return End)