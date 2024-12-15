module Problems.Y2023.Problem8 where

import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Debug.Trace

data Direction = R | L

runEasy :: FilePath -> IO String
runEasy fp = do
    (dirs, mapping) <- parseFile parseInput fp
    let d = runMap 0 "AAA" (cycle dirs) mapping
    return $ show d

runHard :: FilePath -> IO String
runHard fp = do
    (dirs, mapping) <- parseFile parseInput fp
    let starting = filter (\k -> (head $ reverse k) == 'A') $ M.keys mapping
    let indexedDirs = zip [0..] dirs
    let cycles = map (buildCycle (cycle indexedDirs) mapping) starting
    let cycleIndices = map (map fst) cycles
    return $ show $ foldl lcm 1 (map head cycleIndices)

buildCycle :: [(Int, Direction)] -> M.Map String (String, String) -> String -> [(Int, String)]
buildCycle ds m node = let c = buildCycle' ds m S.empty node
                           indexed = zip [0..] c
                       in filter (\(_, k) -> (head $ reverse k) == 'Z') indexed

buildCycle' :: [(Int, Direction)] -> M.Map String (String, String) -> S.Set (String, Int) -> String -> [String]
buildCycle' ((i, d):ds) m s node | S.member (node, i) s = [node]
                                 | otherwise = node:(buildCycle' ds m newSet newNode)
                               where newSet = S.insert (node, i) s
                                     newNode = runMap' d m node

runMaps :: Int -> [String] -> [Direction] -> M.Map String (String, String) -> Int
runMaps i ss (d:ds) m | all (\k -> (head $ reverse k) == 'Z') ss = i
                      | otherwise = runMaps (i + 1) newMaps ds m
                    where newMaps = map (runMap' d m) ss

runMap' :: Direction -> M.Map String (String, String) -> String -> String
runMap' L m s = fst $ Y.fromJust $ M.lookup s m
runMap' R m s = snd $ Y.fromJust $ M.lookup s m

runMap :: Int -> String -> [Direction] -> M.Map String (String, String) -> Int
runMap i "ZZZ" _ _ = i
runMap i node (R:ds) m = runMap (i + 1) r ds m
                     where (l, r) = Y.fromJust $ M.lookup node m
runMap i node (L:ds) m = runMap (i + 1) l ds m
                     where (l, r) = Y.fromJust $ M.lookup node m


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m ([Direction], M.Map String (String, String))
parseInput = do
    instructions <- many $ (char 'L' *> pure L) <|> (char 'R' *> pure R)
    eol
    eol
    nodes <- sepEndBy1 parseNode eol
    return $ (instructions, M.fromList nodes)

parseNode :: (Monad m) => ParsecT Void String m (String, (String, String))
parseNode = do
    name <- some letterChar
    string " = ("
    l <- some letterChar
    string ", "
    r <- some letterChar
    char ')'
    return (name, (l, r))