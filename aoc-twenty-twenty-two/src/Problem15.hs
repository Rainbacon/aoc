module Problem15 where

import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Sensor = (Point, Point)
type Beacons = [Point]
type Equation = (Int, (Int, Int))

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    putStrLn "what row?"
    col <- read <$> getLine
    let knownBeacons = S.elems $ S.fromList $ map snd input
    return $ show $ sum $ map (getSize knownBeacons) $ combineLines $ map Y.fromJust $ filter Y.isJust $ map (findZone col) input

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let eqns = concat $ map findLines input
    let points = map findIntersection eqns <*> eqns
    let (x, y) = Y.fromJust $ head $ filter (Y.fromJust . (fmap inBounds)) $ filter ((testPoint input) . Y.fromJust) $ filter Y.isJust points
    return $ show $ 4000000 * x + y
    


parseInput :: (Monad m) => ParsecT Void String m [Sensor]
parseInput = sepEndBy1 parseSensor eol
    where parseSensor = do
                         string "Sensor at x="
                         x_sen <- parseInt
                         string ", y="
                         y_sen <- parseInt
                         string ": closest beacon is at x="
                         x_beac <- parseInt
                         string ", y="
                         y_beac <- parseInt
                         return ((x_sen, y_sen), (x_beac, y_beac))

findZone :: Int -> Sensor -> Maybe Line
findZone col (s@(sx, sy), (bx, by)) | abs (sy - col) < dist = Just ((sx - dx, col), (sx + dx, col))
                                    | otherwise = Nothing
    where dist = abs (bx - sx) + abs (by - sy)
          dx = dist - (abs (sy - col))

getSize :: Beacons -> Line -> Int
getSize bs l = let numIntersections = filter id $ map (intersects l) bs
               in (length' l) - (length numIntersections)

combine :: Line -> Line -> Maybe Line
combine l1@((x1, y), (x2, _)) l2@((x3, _), (x4, _)) | x1 <= x3 && x3 <= x2 = Just ((x1, y), ((max x2 x4), y))
                                                    | x4 <= x2 && x4 >= x1 = Just ((min x1 x3, y), (x2, y))
                                                    | x1 <= x3 && x2 >= x4 = Just l1
                                                    | x3 <= x1 && x4 >= x2 = Just l2
                                                    | otherwise = Nothing

combine' :: Line -> Line -> Line
combine' x y = case combine x y of
                (Just z) -> z
                Nothing -> y

combineLines :: [Line] -> [Line]
combineLines [] = []
combineLines (x:[]) = [x]
combineLines (x:xs) = let combos = map (combine x) xs
                          combos' = map (combine' x) xs
                          isUnique = length (filter Y.isJust combos) == 0
                      in case isUnique of
                        True -> (x:combineLines combos')
                        False -> combineLines combos'

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findLines :: Sensor -> [Equation]
findLines (s@(x1, y1), b@(x2, y2)) = let radius = (2 + dist s b) `div` 2
                                         points = [(-1, (x1 - radius, y1 - radius)), (-1, (x1 + radius, y1 + radius)), (1, (x1 + radius, y1 - radius)), (1, (x1 - radius, y1 + radius))]
                                     in map makeEqn points

makeEqn :: (Int, Point) -> Equation
makeEqn (slope, (x, y)) = (slope, (0, b))
                    where b = y - (slope * x) 

findIntersection :: Equation -> Equation -> Maybe Point
findIntersection (m1, (_, b1)) (m2, (_, b2)) | m1 == m2 = Nothing
                                             | m1 == 1 = Just ((b2 - b1) `div` 2, b1 + (b2 - b1) `div` 2)
                                             | otherwise = Just ((b1 - b2) `div` 2, b2 + (b1 - b2) `div` 2)

testPoint :: [Sensor] -> Point -> Bool
testPoint sensors point = all (test' point) sensors
    where test' p s = dist p (fst s) > (uncurry dist s)

inBounds :: Point -> Bool
inBounds (x, y) = x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000
