module Problems2025.Problem7 (runEasy, runHard) where

import Utils.Grid
import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Set as S

data Manifold = Start | Split

isStart :: Manifold -> Bool
isStart Start = True
isStart _ = False

charToManifold :: Char -> Maybe Manifold
charToManifold 'S' = Just Start
charToManifold '^' = Just Split
charToManifold _ = Nothing

runEasy :: FilePath -> IO String
runEasy fp = do
    grid <- parseFast parseGrid fp
    let start = fst $ M.elemAt 0 $ M.filter isStart grid
    let bottom = max $ map fst $ M.keys grid
    return $ show $ sum $ fst $ foldl (expand grid) (0, S.singleton $ snd start) [fst start..bottom]

expand :: Grid Manifold -> (Int, S.Set Int) -> Int
expand grid (tot, streams) row

runHard :: FilePath -> IO String
runHard _ = return ""

parseGrid :: String -> Grid Manifold
parseGrid = constructGridSkipEmpty . ((fmap . fmap) charToManifold) . lines