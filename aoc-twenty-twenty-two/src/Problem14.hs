module Problem14 where

import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Line = (Point, Point)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ constructRocks input

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m [Line]
parseInput = concat <$> sepEndBy1 parseLine eol
    where parseLine = do
            points <- sepBy1 parsePoint (string " -> ")
            return $ collectPoints points

parsePoint :: (Monad m) => ParsecT Void String m Point
parsePoint = do
    y <- parseInt
    char ','
    x <- parseInt
    return (x, y)

collectPoints :: [Point] -> [Line]
collectPoints [] = []
collectPoints (x:[]) = error "only one point"
collectPoints (x:y:[]) = [(x,y)]
collectPoints (x:y:xs) = (x,y):(collectPoints (y:xs))

constructRocks :: [Line] -> (Point, [String])
constructRocks xs = let allPoints = concat $ map (\(a, b) -> [a,b]) xs
                        xCoords = map fst allPoints
                        yCoords = map snd allPoints
                        modulate dx ((x1, y1), (x2, y2)) = ((x1, y1 - dx), (x2, y2 - dx))
                        modulation = minimum yCoords
                        points = S.fromList $ concat $ map (extendLine . (modulate modulation)) xs
                        origin = (0, 500 - modulation)
                        genLine x = map (mapChar points origin x) [0..(maximum yCoords - modulation)]
                    in (origin, map genLine [0..maximum xCoords])

extendLine :: Line -> [Point]
extendLine ((x1, y1), (x2, y2)) | x1 == x2 = [(x1, y) | y <- [y1, y1 + (signum $ y2 - y1)..y2]]
                                | otherwise = [(x, y1) | x <- [x1, x1 + (signum $ x2 - x1)..x2]]

mapChar :: S.Set Point -> Point -> Int -> Int -> Char
mapChar points origin x y | S.member (x, y) points = '#'
                          | (x, y) == origin = '+'
                          | otherwise = '.'

dropSand :: Point -> [String] -> Maybe Point
dropSand (x, y) grid | x >= length grid = Nothing
                     | y < 0 || y >= (length $ head grid) = Nothing
                     | (grid !! (x + 1)) !! y == '.' = dropSand (x + 1, y) grid
                     | (grid !! (x + 1)) !! (y - 1) == '.' = dropSand (x + 1, y - 1) grid
                     | (grid !! (x + 1)) !! (y + 1) == '.' = dropSand (x + 1, y + 1) grid
                     | otherwise = Just (x, y)
