module Problems2022.Problem12 where

import qualified Control.Monad.State as ST
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Void
import Utils.Parsing
import Utils

type InputType = (Point, Point, M.Map Point Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- readFile fp
    let (start, end, points) = parseInput $ lines input
    let initialState = ([(start, 0)], S.empty)
    return $ show $ ST.evalState (aStar end points generateNeighbors) initialState

runHard :: FilePath -> IO String
runHard fp = do
    input <- readFile fp
    let (_, end, points) = parseInput $ lines input
    let starts = M.keys $ M.filter (== 1) points
    let initialState = (zip starts (repeat 0), S.empty)
    return $ show $ ST.evalState (aStar end points generateNeighbors) initialState

parseInput :: [String] -> InputType
parseInput xs = let positions = mapPos xs
                    (start, end) = foldl parseCell (Nothing, Nothing) positions
                in (Y.fromJust start, Y.fromJust end, M.fromList $ map (fmap toHeight) positions)

parseCell :: (Maybe Point, Maybe Point) -> (Point, Char) -> (Maybe Point, Maybe Point)
parseCell (s, e) (p, 'S') = (Just p, e)
parseCell (s, e) (p, 'E') = (s, Just p)
parseCell xs _ = xs

toHeight :: Char -> Int
toHeight 'S' = 1
toHeight 'E' = 26
toHeight c = C.ord c - 96

generateNeighbors :: Point -> Grid -> [(Point, Int)]
generateNeighbors (x, y) g = let height = Y.fromJust $ M.lookup (x, y) g
                                 points = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                                 mappedPoints = map (\a -> (a, M.lookup a g)) points 
                                 valid = map (\(p, mv) -> (p, fmap (\i -> (i - height) < 2) mv)) mappedPoints
                             in zip (map fst $ filter (Y.fromJust . snd) $ filter (Y.isJust . snd) valid) (repeat 1)
