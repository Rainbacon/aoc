module Problems2023.Problem21 (runEasy, runHard) where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Tile = Empty | Rock | Start
    deriving (Eq, Ord)
type Garden = M.Map Point Tile

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let available = M.filter (/= Rock) input
    let start = head $ M.keys $ M.filter (== Start) input
    return $ show $ S.size $ runStep 64 (S.singleton start) available

runHard :: FilePath -> IO String
runHard _ = return ""

runStep :: Int -> S.Set Point -> Garden -> S.Set Point
runStep 0 points garden = points
runStep n points garden = runStep (n - 1) newPoints garden
                      where newPoints = S.filter (\p -> M.member p garden) allNeighbors
                            allNeighbors = S.foldl (\acc point -> S.union acc (neighbors point)) S.empty points

neighbors :: Point -> S.Set Point
neighbors (x, y) = S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


--- Parsing ---
parseInput :: ParsecT Void String m Garden
parseInput = do
    rows <- sepEndBy1 parseRow eol
    return $ M.fromList $ mapPos rows

parseRow :: ParsecT Void String m [Tile]
parseRow = some $ (char '.' *> pure Empty) <|> (char '#' *> pure Rock) <|> (char 'S' *> pure Start)