module Problems2023.Problem18 (runEasy, runHard) where

import Control.Lens.Fold
import qualified Data.Maybe as Y
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Numeric.Lens

type Instruction = (CompassDirection, Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let poly = toPoints input
    return $ show $ 1 + ((perimeter poly) `div` 2) + area poly

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseHard fp
    let poly = toPoints input
    return $ show $ 1 + ((perimeter poly) `div` 2) + area poly

toPoints :: [Instruction] -> [Point]
toPoints is = scanl toPoint (0,0) is
          where toPoint (x, y) (N, m) = (x - m, y)
                toPoint (x, y) (S, m) = (x + m, y)
                toPoint (x, y) (E, m) = (x, y + m)
                toPoint (x, y) (W, m) = (x, y - m)

perimeter :: [Point] -> Int
perimeter [] = 0
perimeter (x:[]) = 0
perimeter (x:y:xs) = dist x y + perimeter (y:xs)

area :: [Point] -> Int
area ps = abs $ area' ps `div` 2

area' :: [Point] -> Int
area' [] = 0
area' (x:[]) = 0
area' ((x1,y1):a@(x2,y2):xs) = (x1*y2 - x2*y1) + area' (a:xs)


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [Instruction]
parseInput = sepEndBy1 parseInstruction eol

parseInstruction :: (Monad m) => ParsecT Void String m Instruction
parseInstruction = do
    dir <- (char 'U' *> pure N) <|> (char 'D' *> pure S) <|> (char 'L' *> pure W) <|> (char 'R' *> pure E)
    char ' '
    mag <- parseInt
    char ' '
    string "(#"
    some (digitChar <|> letterChar)
    char ')'
    return (dir, mag)

parseHard :: (Monad m) => ParsecT Void String m [Instruction]
parseHard = sepEndBy1 parseInstruction' eol

parseInstruction' :: (Monad m) => ParsecT Void String m Instruction
parseInstruction' = do
    letterChar
    char ' '
    parseInt
    char ' '
    string "(#"
    m <- count 5 hexDigitChar
    let mag = Y.fromJust $ m ^? hex 
    dir <- (char '0' *> pure E) <|> (char '1' *> pure S) <|> (char '2' *> pure W) <|> (char '3' *> pure N)
    char ')'
    return (dir, mag)