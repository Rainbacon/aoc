module Utils (
    parseFile 
  , parseInt
  , Point
  , Grid
  , aStar
  , mapPos
  , tostr
) where

import Control.Monad.IO.Class
import Control.Monad.State as ST
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec

parseFile :: (MonadIO m) => ParsecT Void String m a -> FilePath -> m a
parseFile parser filepath = do
    input <- liftIO $ readFile filepath
    result <- runParserT parser "Utils.hs" input
    case result of
        Left e -> error $ "Failed to parse: " ++ show e
        Right x -> return x


type Point = (Int, Int)
type Grid = M.Map Point Int

mapPos :: [[a]] -> [(Point, a)]
mapPos xs = concat $ zipWith mapPos' [0..] xs
        where mapPos' i ys = zipWith (\j y -> ((i, j), y)) [0..] ys

type Visited = S.Set Point
type PrioQueue = [(Point, Int)]

aStar :: Point -> Grid -> (Point -> Grid -> [(Point, Int)]) -> ST.State (PrioQueue, Visited) Int 
aStar target grid constructNeighbors = do
    (queue, seen) <- ST.get
    let d@(p@(x, y), distance) = head queue
    if ((x, y) == target)
    then return distance
    else do
        let newPaths = constructNeighbors (x, y) grid
        let unseenPaths = filter (\node -> not $ S.member (fst node) seen) newPaths
        let newQueue = insertPaths (map (fmap (+distance)) unseenPaths) $ tail queue
        ST.put (newQueue, S.insert (x,y) seen)
        aStar target grid constructNeighbors

insertPaths :: [(Point, Int)] -> PrioQueue -> PrioQueue
insertPaths [] q = q
insertPaths (x:xs) q = let rest = insertPaths xs q
                       in insertPath x False rest

insertPath :: (Point, Int) -> Bool -> PrioQueue -> PrioQueue
insertPath x False [] = [x]
insertPath x True [] = []
insertPath a@(p, _) True (b@(x, _):xs) | p == x = xs
                                       | otherwise = b:(insertPath a True xs)
insertPath a@(p, v1) inserted q@(b@(x, v2):xs) | p == x && v1 >= v2 = q
                                               | p == x = a:(insertPath a True xs)
                                               | v2 >= v1 = a:(insertPath a True q)
                                               | otherwise = b:(insertPath a inserted xs)

tostr :: Point -> Grid -> Visited -> String
tostr p g v = let rows = maximum . (map fst) $ M.keys g
                  cols = maximum . (map snd) $ M.keys g
                  cell r c | (r, c) == p = 'H'
                           | S.member (r, c) v = 'X'
                           | otherwise = C.chr . (+96) . Y.fromJust $ M.lookup (r,c) g
                  row r = map (cell r) [0..cols]
              in unlines $ map row [0..rows]

parseInt :: (Monad m) => ParsecT Void String m Int
parseInt = read <$> some digitChar
