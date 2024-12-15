module Problems.Y2024.Problem8 (runEasy, runHard) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y

import Utils
import Utils.Parsing

runEasy :: FilePath -> IO String
runEasy fp = do
    antennas <- Utils.Parsing.parseFast (M.fromList . mapPos . lines) fp
    let frequencies = S.fromList $ M.elems $ M.filter (/= '.') antennas
        frequencyMaps = map (\freq -> M.filter (==freq) antennas) $ S.elems frequencies
    return $ show $ S.size $ S.filter (\p -> Y.isJust $ M.lookup p antennas) $ S.fromList $ concatMap ((concatMap findAntinodes) . pairs . M.keys) frequencyMaps

runHard :: FilePath -> IO String
runHard fp = do
    antennas <- Utils.Parsing.parseFast (M.fromList . mapPos . lines) fp
    let frequencies = S.fromList $ M.elems $ M.filter (/= '.') antennas
        frequencyMaps = map (\freq -> M.filter (==freq) antennas) $ S.elems frequencies
    return $ show $ S.size $ S.fromList $ concatMap ((concatMap (findAntinodes' antennas)) . pairs . M.keys) frequencyMaps

pairs :: (Eq a) => [a] -> [(a,a)]
pairs xs = [(a, b) | a <- xs, b <- xs, a /= b]

findAntinodes :: (Point, Point) -> [Point]
findAntinodes ((x1, y1), (x2, y2)) = let xDist = x2 - x1
                                         yDist = y2 - y1
                                     in [(x1 - xDist, y1 - yDist), (x2 + xDist, y2 + yDist)]

findAntinodes' :: M.Map Point Char -> (Point, Point) -> [Point]
findAntinodes' antennas ((x1, y1), (x2, y2)) = let xDist = x2 - x1
                                                   yDist = y2 - y1
                                                   divisor = gcd (max xDist yDist) (min xDist yDist)
                                                   xStep = xDist `div` divisor
                                                   yStep = yDist `div` divisor
                                                   lessLine = map (\n -> (x1 - n*xStep, y1 - n*yStep)) [0..]
                                                   greatLine = map (\n -> (x1 + n*xStep, y1 + n*yStep)) [0..]
                                                   less = takeWhile (\p -> Y.isJust $ M.lookup p antennas) lessLine
                                                   great = takeWhile (\p -> Y.isJust $ M.lookup p antennas) greatLine
                                               in less ++ great
