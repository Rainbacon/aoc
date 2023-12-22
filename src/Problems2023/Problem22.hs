module Problems2023.Problem22 (runEasy, runHard) where

import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

type Brick = (Point3D, Point3D)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let sorted = sortBy (\((_,_,a), _) ((_,_,b), _) -> compare a b) input 
    let fallen = foldl fall [] sorted
    return $ show $ length $ filter (checkBrick fallen) [0..(length fallen) - 1]

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let sorted = sortBy (\((_,_,a), _) ((_,_,b), _) -> compare a b) input 
    let fallen = foldl fall [] sorted
    return $ show $ sum $ map (checkBrick'' fallen) [0..(length fallen) - 1]

checkBrick :: [Brick] -> Int -> Bool
checkBrick bricks n = let rest = (take n bricks) ++ drop (n + 1) bricks
                      in not $ any id $ map (checkBrick' rest) [0..(length rest) - 1]

checkBrick'' :: [Brick] -> Int -> Int
checkBrick'' bricks n = let rest = (take n bricks) ++ drop (n + 1) bricks
                        in length $ filter id $ map (checkBrick' rest) [0..(length rest) - 1]

checkBrick' :: [Brick] -> Int -> Bool
checkBrick' bricks n = let rest = (take n bricks) ++ drop (n + 1) bricks
                           brick = bricks !! n
                       in canFall rest brick

fall :: [Brick] -> Brick -> [Brick]
fall fallen brick | canFall fallen brick = fall fallen (fall' brick)
                  | otherwise = brick:fallen

canFall :: [Brick] -> Brick -> Bool
canFall fallen ((_, _, 1), _) = False
canFall fallen brick = not $ any id $ map (overlap fallenBrick) fallen
                   where fallenBrick = fall' brick

fall' :: Brick -> Brick
fall' ((x1, y1, z1), (x2, y2, z2)) = ((x1, y1, z1 - 1), (x2, y2, z2 - 1))

overlap :: Brick -> Brick -> Bool
overlap b1@(a, b) b2@(c, d) = let brick1 = [(x, y, z) | x <- [fst3 a..fst3 b], y <- [snd3 a..snd3 b], z <- [thd3 a..thd3 b]]
                                  brick2 = [(x, y, z) | x <- [fst3 c..fst3 d], y <- [snd3 c..snd3 d], z <- [thd3 c..thd3 d]]
                              in S.size (S.intersection (S.fromList brick1) (S.fromList brick2)) > 0

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [Brick]
parseInput = sepEndBy1 parseBrick eol

parseBrick :: (Monad m) => ParsecT Void String m Brick
parseBrick = do
    p1 <- sepBy1 parseInt (char ',')
    char '~'
    p2 <- sepBy1 parseInt (char ',')
    return ((p1 !! 0, p1 !! 1, p1 !! 2), (p2 !! 0, p2 !! 1, p2 !! 2))