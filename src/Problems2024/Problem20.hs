module Problems2024.Problem20 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Control.Applicative as A
import Debug.Trace

data Maze = Wall | Start | End | Empty
    deriving (Eq)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let target = head $ M.keys $ M.filter (== End) input
        start = head $ M.keys $ M.filter (== Start) input
        path = ST.evalState (findPath target start input) S.empty
    return $ show $ applyCheats path input

applyCheats :: [Point] -> M.Map Point Maze -> Int
applyCheats [] grid = 0
applyCheats (p:ps) grid = (length successfulCheats) + applyCheats ps grid
    where possibleCheats = genCheats p grid
          successfulCheats = filter (> 99) cheatValues
          cheatValues = map (value) possibleCheats
          value a@(_, q) = if q `elem` ps then length (takeWhile (/= q) ps) else 0

genCheats :: Point -> M.Map Point Maze -> [(Point, Point)]
genCheats (x, y) grid = filter (\(p1, p2) -> isWall p1 && notWall p2) possibleCheats
    where isWall p = M.member p grid && grid M.! p == Wall
          notWall p = M.member p grid && grid M.! p /= Wall
          possibleCheats = [((x - 1, y), (x - 2, y)), ((x + 1, y), (x + 2, y)), ((x, y - 1), (x, y - 2)), ((x, y + 1), (x, y + 2))]

genNeighbors :: Point -> M.Map Point Maze -> [Point]
genNeighbors (x, y) grid = filter (\p -> M.member p grid && grid M.! p /= Wall) $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

findPath :: Point -> Point -> M.Map Point Maze -> ST.State (S.Set Point) [Point]
findPath target q grid = do
    seen <- ST.get
    if q == target
    then return [q]
    else do
        let neighbor = head $ filter (\p -> S.notMember p seen) $ genNeighbors q grid
        ST.put $ S.insert q seen
        restOfPath <- findPath target neighbor grid
        return $ q:restOfPath

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let target = head $ M.keys $ M.filter (== End) input
        start = head $ M.keys $ M.filter (== Start) input
        path = ST.evalState (findPath target start input) S.empty
    putStrLn $ show $ length path
    return $ show $ applyCheats' path

applyCheats' :: [Point] -> Int
applyCheats' ps = length $ filter (> 99) cheatValues
    where cheatValues = map value possibleCheats
          pointPairs = (map (,) ps) <*> ps
          indices = buildIndices ps 0
          possibleCheats = filter (\(p, q) -> (dist p q) <= 20 && (indices M.! q) > (indices M.! p)) pointPairs 
          value (p, q) = ((indices M.! q) - (indices M.! p)) - (dist p q)

buildIndices :: [Point] -> Int -> M.Map Point Int
buildIndices [] _ = M.empty
buildIndices (x:xs) n = M.insert x n $ buildIndices xs (n + 1)

parseInput :: (Monad m) => ParsecT Void String m (M.Map Point Maze)
parseInput = do 
    grid <- sepEndBy1 parseGridRow eol
    return $ M.fromList $ mapPos grid

parseGridRow :: (Monad m) => ParsecT Void String m [Maze]
parseGridRow = many parseUnit

parseUnit :: (Monad m) => ParsecT Void String m Maze
parseUnit = (char '#' >> return Wall) <|> (char '.' >> return Empty) <|> (char 'S' >> return Start) <|> (char 'E' >> return End)