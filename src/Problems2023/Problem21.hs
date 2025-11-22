module Problems2023.Problem21 (runEasy, runHard) where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import qualified Control.Monad.State as ST
import Utils.Parsing
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

data Tile = Empty | Rock | Start
    deriving (Eq, Ord)
type Garden = M.Map Point Tile
type Seen = M.Map Point Int
type Queue = [(Point, Int)]

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let available = M.filter (/= Rock) input
    let start = head $ M.keys $ M.filter (== Start) input
    return $ show $ S.size $ runStep 64 (S.singleton start) neighbors available

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    input' <- parseFast lines fp
    let maxX = length input'
    let maxY = length $ head input'
    let available = M.filter (/= Rock) input
    let start = head $ M.keys $ M.filter (== Start) input
    return $ show $ S.size $ runStep 512 (S.singleton start) (neighbors' maxX maxY) available

runStep :: Int -> S.Set Point -> (Point -> S.Set Point) -> Garden -> S.Set Point
runStep 0 points fn garden = points
runStep n points fn garden = runStep (n - 1) newPoints fn garden
                         where newPoints = S.filter (\p -> M.member p garden) allNeighbors
                               allNeighbors = S.foldl (\acc point -> S.union acc (fn point)) S.empty points

-- bfs :: ST.State (Seen, Queue, Garden) Seen
-- bfs = do
--     (seen, queue, garden) <- ST.get
--     case queue of
--         [] -> return seen
--         ((x, n):xs) -> do
--             let nbs = filter (\el -> Y.isJust $ M.lookup el seen) $ S.toList $ neighbors x
--             let newQ = map ((,) (n + 1)) $ filter (\el -> Y.isJust $ M.lookup el garden) nbs
--             ST.put (M.insert x n seen, newQ, garden)
--             bfs

neighbors :: Point -> S.Set Point
neighbors (x, y) = S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neighbors' :: Int -> Int -> Point -> S.Set Point
neighbors' maxX maxY (x, y) = S.fromList [(x + 1 `modFn` maxX, y), (x - 1 `modFn` maxX, y), (x, y + 1 `modFn` maxY), (x, y - 1 `modFn` maxY)]

modFn :: Int -> Int -> Int
modFn x n = ((x `mod` n) + n) `mod` n

infixl 7 `modFn`


--- Parsing ---
parseInput :: ParsecT Void String m Garden
parseInput = do
    rows <- sepEndBy1 parseRow eol
    return $ M.fromList $ mapPos rows

parseRow :: ParsecT Void String m [Tile]
parseRow = some $ (char '.' *> pure Empty) <|> (char '#' *> pure Rock) <|> (char 'S' *> pure Start)