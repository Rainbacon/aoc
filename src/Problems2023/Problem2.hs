module Problems2023.Problem2 (runEasy, runHard) where

import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Color = Red | Blue | Green
    deriving (Eq)
-- (Red, Blue, Green)
type Hand = (Int, Int, Int)
type Game = (Int, [Hand])

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ sum $ map fst $ filter (isValid (12, 14, 13)) input

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    return $ show $ sum $ map getPower input

getPower :: Game -> Int
getPower (_, hs) = let minR = maximum $ map red hs
                       minB = maximum $ map blue hs
                       minG = maximum $ map green hs
                    in minR * minB * minG

red :: Hand -> Int
red (r, _, _) = r

blue :: Hand -> Int
blue (_, b, _) = b

green :: Hand -> Int
green (_, _, g) = g

isValid :: Hand -> Game -> Bool
isValid maxColors (_, hs) = all (isValid' maxColors) hs

isValid' :: Hand -> Hand -> Bool
isValid' (maxR, maxB, maxG) (r, b, g) = maxR >= r && maxB >= b && maxG >= g


--- PARSING ---
parseInput :: (Monad m) => ParsecT Void String m [Game]
parseInput = sepEndBy1 parseGame eol

parseGame :: (Monad m) => ParsecT Void String m Game
parseGame = do
             string "Game "
             gameId <- parseInt
             char ':'
             hands <- sepBy1 parseHand (char ';')
             return (gameId, hands)

parseHand :: (Monad m) => ParsecT Void String m Hand
parseHand = do
             colors <- sepBy parseColor (char ',')
             return (findColor Red colors, findColor Blue colors, findColor Green colors)

parseColor :: (Monad m) => ParsecT Void String m (Color, Int)
parseColor = do
              space
              num <- parseInt
              space
              color <- (string "blue" *> pure Blue) <|> (string "red" *> pure Red) <|> (string "green" *> pure Green)
              return (color, num)

findColor :: Color -> [(Color, Int)] -> Int
findColor _ [] = 0
findColor search ((c, i):cs) | c == search = i
                             | otherwise = findColor search cs 