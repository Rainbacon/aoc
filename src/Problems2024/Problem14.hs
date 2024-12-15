module Problems2024.Problem14 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import Debug.Trace
import qualified Data.Set as S

type Robot = (Point, (Int, Int))

runEasy :: FilePath -> IO String
runEasy fp = do
    robots <- parseFile parseInput fp
    let maxX = 101
        maxY = 103
    return $ show $ product $ counts $ quadrants maxX maxY $ map (move 100 maxX maxY) robots

move :: Int -> Int -> Int -> Robot -> Robot
move numIters xMod yMod ((x, y), (xVel, yVel)) = ((newX, newY), (xVel, yVel))
    where newX = (x + (numIters * xVel)) `mod` xMod
          newY = (y + (numIters * yVel)) `mod` yMod

quadrants :: Int -> Int -> [Robot] -> [Int]
quadrants maxX maxY robots = map detQuad robots
    where detQuad ((x, y), _) | top && left = 1
                              | top && right = 2
                              | bottom && right = 3
                              | bottom && left = 4
                              | otherwise = 0
                            where top = y < (maxY `div` 2)
                                  bottom = y > (maxY `div` 2)
                                  left = x < (maxX `div` 2)
                                  right = x > (maxX `div` 2)

counts :: [Int] -> [Int]
counts xs = map (count xs) [1..4]
    where count ys n = length $ filter (==n) ys

runHard :: FilePath -> IO String
runHard fp = do
    robots <- parseFile parseInput fp
    let maxX = 101
        maxY = 103
        safety rs = product $ counts $ quadrants maxX maxY rs
    return $ show $ head $ sortBy (\(_, a) (_, b) -> compare a b) $ map (\i -> (i, safety $ map (move i maxX maxY) robots)) [1..100000]

printGrid :: [Robot] -> IO ()
printGrid robots = do
    let robotPoints = S.fromList $ map fst robots
        formLine y = map (\x -> if S.member (x, y) robotPoints then 'X' else '.') [0..100]
        ls = map formLine [0..102]
    mapM_ putStrLn ls

parseInput :: (Monad m) => ParsecT Void String m [Robot]
parseInput = sepEndBy1 parseRobot eol

parseRobot :: (Monad m) => ParsecT Void String m Robot
parseRobot = do
    _ <- string "p="
    position <- parsePair
    _ <- string " v="
    velocity <- parsePair
    return (position, velocity)

parsePair :: (Monad m) => ParsecT Void String m (Int, Int)
parsePair = do
    x <- parseInt
    _ <- char ','
    y <- parseInt
    return (x, y)