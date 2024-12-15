module Problems.Y2023.Problem14 where

import Data.List
import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Debug.Trace

data Tile = Empty | Rock | Roll
    deriving (Eq, Ord)

instance Show Tile where
    show Empty = "."
    show Rock = "#"
    show Roll = "O"

isEmpty :: Tile -> Bool
isEmpty = (==) Empty

isRock :: Tile -> Bool
isRock = (==) Rock

isRoll :: Tile -> Bool
isRoll = (==) Roll

type Plane = M.Map Point Tile
type Cycle = [Plane]

printPlane :: Plane -> (Int, Int) -> String
printPlane plane (mX, mY) = let plane' = fillPlane plane mX mY
                                grid = groupBy (\((x1, _), _) ((x2, _), _) -> x1 == x2) $ M.toAscList plane'
                            in unlines $ map (\r -> concat $ map (snd . (fmap show)) r) grid

fillPlane :: Plane -> Int -> Int -> Plane
fillPlane plane x y = let points = [(a, b) | a <- [0..x], b <- [0..y]]
                      in foldl (\p point -> M.insertWith (curry snd) point Empty p) plane points

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let rolled = M.mapKeys (roll input N (0, 0)) input
    let rolls = M.filter isRoll rolled
    let max = maximum $ map fst $ M.keys input
    return $ show $ sum $ map (\(x, y) -> max - x + 1) $ M.keys rolls

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let maxX = maximum $ map fst $ M.keys input
    let maxY = maximum $ map snd $ M.keys input
    let rolled = roll' input (maxX, maxY)
    let initialState = ([rolled], (maxX, maxY))
    let dirs = cycle [N, E, S, W]
    let (cyc, _) = ST.execState (runCycle 0) initialState
    let cyc' = reverse $ onlyCycle cyc 
    let l = length cyc'
    let initL = length cyc - l
    let cl = (1000000000 - initL) `mod` l - 1
    let final = cyc' !! cl
    putStrLn $ "Modular index: " ++ show cl
    putStrLn $ "Cycle length: " ++ show l
    putStrLn $ "Pre-Cycle length: " ++ show initL
    return $ show $ calcLoad maxX final

calcLoad :: Int -> Plane -> Int
calcLoad mx plane = sum $ map (\(x, y) -> mx - x + 1) $ M.keys $ M.filter isRoll plane

onlyCycle :: Cycle -> Cycle
onlyCycle cyc = take (length cyc' + 1) cyc
              where target = head cyc
                    cyc' = takeWhile (/= target) (tail cyc)

runCycle :: Int -> ST.State (Cycle, Point) ()
runCycle n = do
    (cyc, ms) <- ST.get
    let plane = head cyc
    let rolled = trace ("Cycle " ++ show n) $ roll' plane ms
    ST.put (rolled : cyc, ms)
    if rolled `elem` cyc
    then return ()
    else runCycle (n + 1)

roll' :: Plane -> Point -> Plane
roll' plane ms = foldl (\acc d -> M.mapKeys (roll acc d ms) acc) plane [N, W, S, E]

roll :: Plane -> CompassDirection -> Point -> Point -> Point
roll plane N m p = let tile = Y.fromJust $ M.lookup p plane
                       col = M.filterWithKey (\k _ -> snd k == snd p && fst k < fst p) plane
                       rocks = M.toDescList $ M.filter isRock col
                       rock = case length rocks of
                          0 -> (0, snd p)
                          _ -> fst $ head rocks
                       rolls = M.filterWithKey (\k _ -> fst k >= fst rock) col
                       newPos = (fst rock + length rolls, snd p)
                   in case tile of
                     Roll -> newPos
                     _ -> p
roll plane S m p = let tile = Y.fromJust $ M.lookup p plane
                       col = M.filterWithKey (\k _ -> snd k == snd p && fst k > fst p) plane
                       rocks = M.toAscList $ M.filter isRock col
                       rock = case length rocks of
                          0 -> (fst m, snd p)
                          _ -> fst $ head rocks
                       rolls = M.filterWithKey (\k _ -> fst k <= fst rock) col
                       newPos = (fst rock - length rolls, snd p)
                   in case tile of
                     Roll -> newPos
                     _ -> p
roll plane W m p = let tile = Y.fromJust $ M.lookup p plane
                       row = M.filterWithKey (\k _ -> fst k == fst p && snd k < snd p) plane
                       rocks = M.toDescList $ M.filter isRock row
                       rock = case length rocks of
                          0 -> (fst p, 0)
                          _ -> fst $ head rocks
                       rolls = M.filterWithKey (\k _ -> snd k >= snd rock) row
                       newPos = (fst p, snd rock + length rolls)
                   in case tile of
                     Roll -> newPos
                     _ -> p
roll plane E m p = let tile = Y.fromJust $ M.lookup p plane
                       row = M.filterWithKey (\k _ -> fst k == fst p && snd k > snd p) plane
                       rocks = M.toAscList $ M.filter isRock row
                       rock = case length rocks of
                          0 -> (fst p, snd m)
                          _ -> fst $ head rocks
                       rolls = M.filterWithKey (\k _ -> snd k <= snd rock) row
                       newPos = (fst p, snd rock - length rolls)
                   in case tile of
                     Roll -> newPos
                     _ -> p

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m Plane
parseInput = do
    rows <- sepEndBy1 parseRow eol
    return $ M.filter (not . isEmpty) $ M.fromList $ mapPos rows

parseRow :: (Monad m) => ParsecT Void String m [Tile]
parseRow = some parseTile

parseTile :: (Monad m) => ParsecT Void String m Tile
parseTile = (char 'O' *> pure Roll) <|> (char '#' *> pure Rock) <|> (char '.' *> pure Empty)