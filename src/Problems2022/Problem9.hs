module Problems2022.Problem9 where

import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing


type Position = (Int, Int)
data Rope = Rope [Position]
data Dir = R | L | U | D
    deriving (Read)
type Move = (Dir, Int)
type Visited = S.Set Position
type Screen = [String]

instance Show Rope where
    show (Rope ps) = let screen = replicate 21 (replicate 26 '.')
                     in unlines $ foldl setPixel screen (zip [0..] ps)

showTail :: Visited -> String
showTail v = let screen = replicate 26 (replicate 21 '.')
             in unlines $ foldl setPixel screen (zip [0..] (S.elems v))

setPixel :: Screen -> (Int, Position) -> Screen
setPixel s (n, (x, y)) = let row = s !! x
                             newRow = (take y row) ++ "#" ++ (drop (y + 1) row)
                             alreadySet = (row !! y) /= '.'
                         in case alreadySet of
                            False -> (take x s) ++ [newRow] ++ (drop (x + 1) s)
                            True -> s

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let init = Rope [(0,0), (0,0)]
    let visited = S.empty
    return $ show $ S.size . snd $ foldl runMove (init, visited) input

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let init = Rope (replicate 10 (15,11))
    let visited = S.empty
    return $ show $ S.size . snd $ foldl runMove (init, visited) input

parseInput :: (Monad m) => ParsecT Void String m [Move]
parseInput = sepEndBy1 parseMove eol
         where parseMove = do
                            dir <- parseD <|> parseU <|> parseR <|> parseL
                            char ' '
                            dis <- parseInt
                            return (dir, dis)

parseD :: (Monad m) => ParsecT Void String m Dir
parseD = do
          char 'D'
          return D

parseU :: (Monad m) => ParsecT Void String m Dir
parseU = do
          char 'U'
          return U

parseL :: (Monad m) => ParsecT Void String m Dir
parseL = do
          char 'L'
          return L

parseR :: (Monad m) => ParsecT Void String m Dir
parseR = do
          char 'R'
          return R

runMove :: (Rope, Visited) -> Move -> (Rope, Visited)
runMove s (d, n) = foldl (runMove' d) s [1..n]
               where runMove' dir (r, v) _ = let newR@(Rope rs) = move r dir
                                                 newT = head $ reverse rs
                                             in (newR, S.insert newT v)

move :: Rope -> Dir -> Rope
move (Rope []) d = error "rope is empty"
move (Rope ((xh, yh):rs)) d = let newHead = case d of
                                    U -> (xh, yh + 1) 
                                    D -> (xh, yh - 1) 
                                    L -> (xh - 1, yh) 
                                    R -> (xh + 1, yh) 
                              in Rope $ scanl touch newHead rs

touch :: Position -> Position -> Position
touch (x1, y1) (x2, y2) | x1 - x2 > 1 && y1 == y2 = (x1 - 1, y2)
                        | x1 - x2 > 1 && y1 > y2 = (x1 - 1, y2 + 1)
                        | x1 - x2 > 1 && y1 < y2 = (x1 - 1, y2 - 1)
                        | x2 - x1 > 1 && y1 == y2 = (x1 + 1, y2)
                        | x2 - x1 > 1 && y1 > y2 = (x1 + 1, y2 + 1)
                        | x2 - x1 > 1 && y1 < y2 = (x1 + 1, y2 - 1)
                        | y1 - y2 > 1 && x1 == x2 = (x1, y1 - 1)
                        | y1 - y2 > 1 && x1 > x2 = (x2 + 1, y1 - 1)
                        | y1 - y2 > 1 && x1 < x2 = (x2 - 1, y1 - 1)
                        | y2 - y1 > 1 && x1 == x2 = (x1, y1 + 1)
                        | y2 - y1 > 1 && x1 > x2 = (x2 + 1, y1 + 1)
                        | y2 - y1 > 1 && x1 < x2 = (x2 - 1, y1 + 1)
                        | otherwise = (x2, y2)
