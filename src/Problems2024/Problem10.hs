module Problems2024.Problem10 (runEasy, runHard) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad.State as ST
import Utils

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast (map (map (read . (:[]))) . lines) fp
    let trails = M.fromList $ mapPos input
        trailHeads = M.keys $ M.filter (==0) trails
    return $ show $ sum $ map (\t -> ST.evalState (findTrails t trails) S.empty) trailHeads

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast (map (map (read . (:[]))) . lines) fp
    let trails = M.fromList $ mapPos input
        trailHeads = M.keys $ M.filter (==0) trails
    return $ show $ sum $ map (\t -> findTrails' t trails) trailHeads

findTrails :: Point -> M.Map Point Int -> ST.State (S.Set Point) Int
findTrails p trails = do
    seen <- ST.get
    if S.member p seen
    then return 0
    else do
        ST.put $ S.insert p seen
        let isTarget = 9 == (Y.fromJust $ M.lookup p trails)
        if isTarget
        then return 1
        else do
            let paths = neighbors p trails
            continuation <- mapM (\path -> findTrails path trails) paths
            return $ sum continuation

findTrails' :: Point -> M.Map Point Int -> Int
findTrails' p trails | isTarget = 1
                     | length paths == 0 = 0
                     | otherwise = sum continuation
                   where isTarget = 9 == (Y.fromJust $ M.lookup p trails)
                         paths = neighbors p trails
                         continuation = map (\path -> findTrails' path trails) paths


neighbors :: Point -> M.Map Point Int -> [Point]
neighbors p@(x, y) trails = filter isStep potential
                        where potential = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                              current = Y.fromJust $ M.lookup p trails
                              isStep n = let val = M.lookup n trails
                                             defined = Y.isJust val
                                         in defined && (Y.fromJust val) - current == 1