module Problems2023.Problem23 (runEasy, runHard) where

import qualified Data.Maybe as Y
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

data Nature = Path | Slope CompassDirection | Forest
  deriving (Eq, Show)
type Trails = M.Map Point Nature

isSlope :: Nature -> Bool
isSlope (Slope _) = True
isSlope _ = False

runEasy :: FilePath -> IO String
runEasy fp = do
  input <- parseFile parseInput fp
  let points = mapPos input
  let paths = filter ((== Path) . snd) $ sortBy (\(a, _) (b, _) -> compare a b) points
  let start = fst $ head paths 
  let end = fst $ head $ reverse paths
  let trails = M.fromList $ filter ((/= Forest) . snd) points
  return $ show $ (maximum $ map S.size $ findPaths S.empty trails end constructNeighbors start) - 1

runHard :: FilePath -> IO String
runHard fp = do
  input <- parseFile parseInput fp
  let points = mapPos input
  let paths = filter ((== Path) . snd) $ sortBy (\(a, _) (b, _) -> compare a b) points
  let start = fst $ head paths 
  let end = fst $ head $ reverse paths
  let trails = M.fromList $ filter ((/= Forest) . snd) points
  return $ show $ (maximum $ map S.size $ findPaths S.empty trails end constructNeighbors' start) - 1

findPaths :: S.Set Point -> Trails -> Point -> (Trails -> Point -> [Point]) -> Point -> [S.Set Point]
findPaths seen trails target fn current | current == target = [newSeen]
                                        | S.member current seen = []
                                        | otherwise = concat $ map (findPaths newSeen trails target fn) neighbors
                                      where newSeen = S.insert current seen
                                            neighbors = fn trails current

constructNeighbors :: Trails -> Point -> [Point]
constructNeighbors trails p@(x, y) | isSlope current = takeSlope p current
                                   | otherwise = map snd $ filter canTravel possibles
                                 where possibles = [(S, (x + 1, y)), (N, (x - 1, y)), (E, (x, y + 1)), (W, (x, y - 1))]
                                       invalidPairs = [(S, N), (N, S), (E, W), (W, E)]
                                       current = Y.fromJust $ M.lookup p trails
                                       canTravel (dir, point) = case M.lookup point trails of
                                        (Just (Slope d)) -> not $ (dir, d) `elem` invalidPairs
                                        Nothing -> False
                                        _ -> True

constructNeighbors' :: Trails -> Point -> [Point]
constructNeighbors' trails p@(x, y) = map snd $ filter canTravel possibles
                                  where possibles = [(S, (x + 1, y)), (N, (x - 1, y)), (E, (x, y + 1)), (W, (x, y - 1))]
                                        current = Y.fromJust $ M.lookup p trails
                                        canTravel (dir, point) = case M.lookup point trails of
                                          Nothing -> False
                                          _ -> True

takeSlope :: Point -> Nature -> [Point]
takeSlope (x, y) (Slope N) = [(x - 1, y)]
takeSlope (x, y) (Slope S) = [(x + 1, y)]
takeSlope (x, y) (Slope E) = [(x, y + 1)]
takeSlope (x, y) (Slope W) = [(x, y - 1)]
takeSlope _ _ = error "you done messed up"


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [[Nature]]
parseInput = sepEndBy1 parseRow eol

parseRow :: (Monad m) => ParsecT Void String m [Nature]
parseRow = many parseNature

parseNature :: (Monad m) => ParsecT Void String m Nature
parseNature = (char '.' *> pure Path) <|> (char '#' *> pure Forest) <|> parseSlope

parseSlope :: (Monad m) => ParsecT Void String m Nature
parseSlope = (char '^' *> pure (Slope N)) <|> (char '>' *> pure (Slope E)) <|> (char '<' *> pure (Slope W)) <|> (char 'v' *> pure (Slope S))