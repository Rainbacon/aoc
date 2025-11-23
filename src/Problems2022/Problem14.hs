module Problems2022.Problem14 where

import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Utils

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let st = constructRocks input
    return $ show $ length $ takeWhile (Y.isJust . fst) $ scanl dropSand st [0..]

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let floor = findFloor input
    let st = constructRocks (floor:input)
    return $ show $ length $ takeWhile (not . isSand) $ scanl dropSand st [0..]

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

isSand :: (Maybe Point, [String]) -> Bool
isSand (Nothing, _) = True
isSand ((Just (x, y)), grid) = (grid !! x) !! y == 'o'

findFloor :: [Line] -> Line
findFloor xs = let allPoints = concat $ map (\(a, b) -> [a,b]) xs
                   xCoords = map fst allPoints
                   yCoords = map snd allPoints
                   height = maximum xCoords + 2
                   width = 2 * height - 1
                   minY = min (minimum yCoords - 5) (500 - width)
                   maxY = max (maximum yCoords + 5) (500 + width)
               in ((height, minY), (height, maxY))

constructRocks :: [Line] -> (Maybe Point, [String])
constructRocks xs = let allPoints = concat $ map (\(a, b) -> [a,b]) xs
                        xCoords = map fst allPoints
                        yCoords = map snd allPoints
                        modulate dx ((x1, y1), (x2, y2)) = ((x1, y1 - dx), (x2, y2 - dx))
                        modulation = minimum yCoords
                        points = S.fromList $ concat $ map (extendLine . (modulate modulation)) xs
                        origin = (0, 500 - modulation)
                        genLine x = map (mapChar points origin x) [0..(maximum yCoords - modulation)]
                    in (Just origin, map genLine [0..maximum xCoords])

extendLine :: Line -> [Point]
extendLine ((x1, y1), (x2, y2)) | x1 == x2 = [(x1, y) | y <- [y1, y1 + (signum $ y2 - y1)..y2]]
                                | otherwise = [(x, y1) | x <- [x1, x1 + (signum $ x2 - x1)..x2]]

mapChar :: S.Set Point -> Point -> Int -> Int -> Char
mapChar points origin x y | S.member (x, y) points = '#'
                          | (x, y) == origin = '+'
                          | otherwise = '.'

dropSand :: (Maybe Point, [String]) -> a -> (Maybe Point, [String])
dropSand (Nothing, grid) _ = (Nothing, grid)
dropSand ((Just p), grid) _ = let landing = dropSand' p grid
                              in case landing of
                                Nothing -> (Nothing, grid)
                                (Just q) -> ((Just p), updateGrid grid q)

dropSand' :: Point -> [String] -> Maybe Point
dropSand' (x, y) grid | x >= length grid = Nothing
                     | y < 0 || y >= (length $ head grid) = Nothing
                     | otherwise = case tryMove grid (x, y) of
                                        Nothing -> Just (x, y)
                                        (Just p) -> dropSand' p grid

tryMove :: [String] -> Point -> Maybe Point
tryMove grid (x, y) = tryMove' grid (x + 1, y) <|> tryMove' grid (x + 1, y - 1) <|> tryMove' grid (x + 1,y + 1)


tryMove' :: [String] -> Point -> Maybe Point
tryMove' grid p@(x, y) | x < 0 || x >= length grid = Just p
                       | y < 0 || y >= (length $ head grid) = Just p
                       | (grid !! x) !! y == '.' = Just p
                       | otherwise = Nothing

updateGrid :: [String] -> Point -> [String]
updateGrid grid (x, y) = let rowsBefore = take x grid
                             row = grid !! x
                             rowsAfter = drop (x + 1) grid
                             colsBefore = take y row
                             colsAfter = drop (y + 1) row
                         in rowsBefore ++ [colsBefore ++ "o" ++ colsAfter] ++ rowsAfter
