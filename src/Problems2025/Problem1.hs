module Problems2025.Problem1 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Turn = LTurn Int | RTurn Int

extract :: Turn -> Int
extract (LTurn i) = i
extract (RTurn i) = i

click :: Turn -> Turn
click (LTurn i) = LTurn (i - 1)
click (RTurn i) = RTurn (i - 1)

runEasy :: FilePath -> IO String
runEasy fp = do
    turns <- parseFile (sepEndBy1 parseTurn eol) fp
    return $ show $ length $ filter (== 0) $ scanl applyTurn 50 turns

applyTurn :: Int -> Turn -> Int
applyTurn i (LTurn j) = (i - j) `mod` 100
applyTurn i (RTurn j) = (i + j) `mod` 100

runHard :: FilePath -> IO String
runHard fp = do
    turns <- parseFile (sepEndBy1 parseTurn eol) fp
    return $ show $ length $ filter (== 0) $ foldl applyTurn' [50] turns

applyTurn' :: [Int] -> Turn -> [Int]
applyTurn' [] _ = error "applied turn prime with empty dial"
applyTurn' all@(x:xs) turn | extract turn == 0 = all
                           | otherwise = applyTurn' iterated turned
                         where turned = click turn
                               iterated = (applyTurn'' x turn):all
                               applyTurn'' a (LTurn _) = (a - 1) `mod` 100
                               applyTurn'' a (RTurn _) = (a + 1) `mod` 100

crosses :: Int -> Turn -> Bool
crosses 0 _ = False
crosses i (LTurn j) = j >= i
crosses i (RTurn j) = (100 - j) <= i
          

parseTurn :: (Monad m) => ParsecT Void String m Turn 
parseTurn = do
    constructor <- parseLeft <|> parseRight
    mag <- parseInt
    return $ constructor mag

parseLeft :: (Monad m) => ParsecT Void String m (Int -> Turn) 
parseLeft = char 'L' >> pure LTurn

parseRight :: (Monad m) => ParsecT Void String m (Int -> Turn) 
parseRight = char 'R' >> pure RTurn