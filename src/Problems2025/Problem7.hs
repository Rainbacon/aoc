module Problems2025.Problem7 (runEasy, runHard) where

import Utils.Grid
import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST 
import Debug.Trace

data Manifold = Start | Split | Empty

type ManifoldState = (Grid Manifold, S.Set Point, S.Set Point)
type ManifoldState' = (Grid Manifold, [Point], M.Map Point Int)

isStart :: Manifold -> Bool
isStart Start = True
isStart _ = False

charToManifold :: Char -> Manifold
charToManifold 'S' = Start
charToManifold '^' = Split
charToManifold '.' = Empty
charToManifold c = error ("No manifold constructor matches char " ++ [c])

runEasy :: FilePath -> IO String
runEasy fp = do
    grid <- parseFast parseGrid fp
    let start = fst $ M.elemAt 0 $ M.filter isStart grid
    let initialState = (grid, (S.singleton start), S.empty)
    return $ show $ ST.evalState runManifold initialState

runManifold :: ST.State ManifoldState Int
runManifold = do
    (grid, tachyons, seen) <- ST.get
    case S.size tachyons of
        0 -> return 0
        otherwise -> do
            next <- popTachyon
            splits <- expandTachyon next
            rest <- runManifold
            return $ splits + rest

expandTachyon :: Point -> ST.State ManifoldState Int
expandTachyon p@(x, y) = do
    (grid, tachyons, seen) <- ST.get
    case S.member p seen of
        True -> return 0
        False -> do
            let next = (x + 1, y)
            case M.lookup next grid of
                Nothing -> return 0
                (Just Split) -> do
                    ST.put (grid, S.union tachyons (S.fromList [(x + 1, y - 1), (x + 1, y + 1)]), S.insert p seen)
                    return 1
                otherwise -> do
                    ST.put (grid, S.insert next tachyons, S.insert p seen)
                    return 0


popTachyon :: ST.State ManifoldState Point
popTachyon = do
    (grid, tachyons, seen) <- ST.get
    case S.size tachyons of
        0 -> error "popped from empty list"
        otherwise -> do
            ST.put (grid, S.deleteAt 0 tachyons, seen)
            return $ S.elemAt 0 tachyons

runHard :: FilePath -> IO String
runHard fp = do
    grid <- parseFast parseGrid fp
    let start = fst $ M.elemAt 0 $ M.filter isStart grid
    return $ show $ runManifold' grid start

runManifold' :: Grid Manifold -> Point -> Int
runManifold' g p = M.findWithDefault 1 p $ M.mapWithKey runManifold'' g
    where runManifold'' (x, y) Split = runManifold' g (x, y - 1) + runManifold' g (x, y + 1) 
          runManifold'' (x, y) _ = runManifold' g (x + 1, y)

parseGrid :: String -> Grid Manifold
parseGrid = constructGrid . ((fmap . fmap) charToManifold) . lines