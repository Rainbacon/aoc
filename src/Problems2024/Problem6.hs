module Problems2024.Problem6 (runEasy, runHard) where

import Control.Monad.State as ST
import Utils.Parsing
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S

data Direction = U | D | L | R
    deriving (Eq, Ord)

next :: Direction -> Direction
next U = R
next R = D
next D = L
next L = U

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast (M.fromList . mapPos . lines) fp
    let guard = fst $ head $ M.toList $ M.filter (== '^') input
    return $ show $ S.size $ S.fromList $ runGuard guard U input

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast (M.fromList . mapPos . lines) fp
    let emptyPoints = M.keys $ M.filter (/= '#') input
    let guard = fst $ head $ M.toList $ M.filter (== '^') input
    return $ show $ length $ filter id $ map (findLoop guard input) emptyPoints

runGuard :: Point -> Direction -> M.Map Point Char -> [Point]
runGuard p d m = let point = M.lookup nextPoint m
                     nextPoint = nextPos p d
                     turn = next d
                 in case point of
                    Nothing -> [p]
                    (Just '#') -> (p:(runGuard p turn m))
                    (Just _) -> (p:(runGuard nextPoint d m))

nextPos :: Point -> Direction -> Point
nextPos (x, y) U = (x - 1, y) 
nextPos (x, y) D = (x + 1, y) 
nextPos (x, y) L = (x, y - 1) 
nextPos (x, y) R = (x, y + 1)



type LoopState = S.Set (Point, Direction)

findLoop :: Point -> M.Map Point Char -> Point -> Bool
findLoop guardPoint m testPoint = let addedPoint = M.adjust (\_ -> '#') testPoint m
                                  in ST.evalState (findLoop' addedPoint (Just (guardPoint, U))) S.empty

findLoop' :: M.Map Point Char -> Maybe (Point, Direction) -> ST.State LoopState Bool
findLoop' _ Nothing = return False
findLoop' m (Just (p, d)) = do
    seen <- ST.get
    let isLoop = S.member (p, d) seen
    ST.put $ S.insert (p, d) seen
    if isLoop
    then return True
    else findLoop' m (runGuard' p d m)

runGuard' :: Point -> Direction -> M.Map Point Char -> Maybe (Point, Direction)
runGuard' p d m = let point = M.lookup nextPoint m
                      nextPoint = nextPos p d
                      turn = next d
                  in case point of
                    Nothing -> Nothing
                    (Just '#') -> Just (p, turn)
                    (Just _) -> Just (nextPoint, d)