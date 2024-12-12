module Problems2024.Problem12 (runEasy, runHard) where

import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad.State as ST
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import GHC.Utils.Monad
import Debug.Trace

data Region = Region {
    plant :: Char
  , points :: S.Set Point
}

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let regions = ST.evalState (expandRegions input) S.empty
    return $ show $ sum $ map cost regions 

cost :: Region -> Int
cost (Region a ps) = perim * area
                 where area = S.size ps
                       perim = sum $ map perim' (S.elems ps)
                       perim' p = length $ filter (\x -> S.notMember x ps) $ neighbors p

expandRegions :: M.Map Point Char -> ST.State (S.Set Point) [Region]
expandRegions m = do
    maybeRegions <- mapM (expandRegion' m) $ M.keys m
    return $ map Y.fromJust $ filter (Y.isJust) maybeRegions

expandRegion' :: M.Map Point Char -> Point -> ST.State (S.Set Point) (Maybe Region)
expandRegion' m p = do
    seen <- ST.get
    if S.member p seen
    then return Nothing
    else do
        let c = Y.fromJust $ M.lookup p m
        region <- expandRegion c m p
        return (Just (Region c (S.fromList region)))

expandRegion :: Char -> M.Map Point Char -> Point -> ST.State (S.Set Point) [Point]
expandRegion c m p = do
    seen <- ST.get
    let currentPlant = M.lookup p m
    if S.member p seen || Y.isNothing currentPlant || Y.fromJust currentPlant /= c
    then return []
    else do
        ST.put $ S.insert p seen
        rest <- concatMapM (expandRegion c m) $ neighbors p
        return (p:rest)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let regions = ST.evalState (expandRegions input) S.empty
    return $ show $ sum $ map cost' regions 

cost' :: Region -> Int
cost' (Region _ ps) = area * numSides
                  where area = S.size ps
                        numSides = sum $ map (countCorners ps) $ S.elems ps

countCorners :: S.Set Point -> Point -> Int
countCorners ps (x, y) = length $ filter id [tl_vex, tr_vex, br_vex, bl_vex, tl_cave, tr_cave, bl_cave, br_cave]
                     where tl_vex = all (\p -> S.notMember p ps) [(x, y - 1), (x - 1, y)]
                           tr_vex = all (\p -> S.notMember p ps) [(x, y + 1), (x - 1, y)]
                           br_vex = all (\p -> S.notMember p ps) [(x, y + 1), (x + 1, y)]
                           bl_vex = all (\p -> S.notMember p ps) [(x, y - 1), (x + 1, y)]
                           tl_cave = S.notMember (x - 1, y - 1) ps && all (\p -> S.member p ps) [(x, y - 1), (x - 1, y)]
                           tr_cave = S.notMember (x - 1, y + 1) ps && all (\p -> S.member p ps) [(x, y + 1), (x - 1, y)]
                           br_cave = S.notMember (x + 1, y + 1) ps && all (\p -> S.member p ps) [(x, y + 1), (x + 1, y)]
                           bl_cave = S.notMember (x + 1, y - 1) ps && all (\p -> S.member p ps) [(x, y - 1), (x + 1, y)]

parseInput :: (Monad m) => ParsecT Void String m (M.Map Point Char)
parseInput = do
    inputLines <- sepEndBy1 (many letterChar) eol
    return $ M.fromList $ mapPos inputLines