module Problem15 where

import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Sensor = (Point, Point)
type Beacons = [Point]

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
    let (s1, s2) = findPair input
    let (x, y) = findPoint s1 s2
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

findPair :: [Sensor] -> (Sensor, Sensor)
findPair xs = let sensorPairs = map checkPair xs <*> xs
              in snd $ head $ filter (id . fst) sensorPairs

checkPair :: Sensor -> Sensor -> (Bool, (Sensor, Sensor))
checkPair s1 s2 = (distSensors == (2 + d1 + d2), (s1, s2))
    where distSensors = dist (fst s1) (fst s2)
          d1 = uncurry dist s1
          d2 = uncurry dist s2

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findPoint :: Sensor -> Sensor -> Point
findPoint (p@(x1, y1), b) ((x2, y2), _) = let ds = (x2 - x1, y2 - y1)
                                              slope = findSlope ds
                                              r1 = dist p b
                                              line = scanl (add slope) p [0..]
                                        in head $ dropWhile ((<= r1) . (dist p)) line

findSlope :: (Int, Int) -> (Int, Int)
findSlope (x, y) | x > y = (x `div` y, 1)
                 | otherwise = (1, y `div` x)

add :: (Int, Int) -> Point -> Int -> Point
add (dx, dy) (x, y) _ = (x + dx, y + dy)
