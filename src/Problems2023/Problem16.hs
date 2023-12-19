module Problems2023.Problem16 where

import qualified Control.Monad.State as ST
import qualified Data.Set as S
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

data Tile = Empty | HSplit | VSplit | AngleD | AngleU
    deriving (Eq)
type LaserState = (S.Set (Point, CompassDirection), [[Tile]])

toChar :: Tile -> Char
toChar Empty = '.'
toChar HSplit = '-'
toChar VSplit = '|'
toChar AngleD = '\\'
toChar AngleU = '/'

showBeams :: S.Set Point -> [[Tile]] -> String
showBeams beams grid = let rowed = zip [0..] grid
                           showBeam row col t | S.member (row, col) beams = '#'
                                              | otherwise = toChar t
                       in unlines $ map (\(r, row) -> zipWith (showBeam r) [0..] row) rowed

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let (energized, _) = ST.execState (runLaser (0,0) E) (S.empty, input)
    return $ show $ S.size $ S.map fst energized

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let x = length input
    let y = length $ head input
    let north = map (\n -> ((x - 1, n), N)) [0..y]
    let south = map (\n -> ((0, n), S)) [0..y]
    let east = map (\n -> ((n, 0), E)) [0..x]
    let west = map (\n -> ((n, y - 1), W)) [0..x]
    return $ show $ maximum $ parallelMap (S.size . uncurry (execLaser input)) (north ++ south ++ east ++ west)


execLaser :: [[Tile]] -> Point -> Dir -> S.Set Point
execLaser grid p d = let (energized, _) = ST.execState (runLaser p d) (S.empty, grid)
                     in S.map fst energized

runLaser :: Point -> CompassDirection -> ST.State LaserState ()
runLaser p@(x, y) d = do
    (energized, grid) <- ST.get
    if not (inBounds grid p) || S.member (p, d) energized
    then return ()
    else do
        let continuations = next p d grid
        ST.put (S.insert (p, d) energized, grid)
        mapM_ (uncurry runLaser) continuations


next :: Point -> CompassDirection -> [[Tile]] -> [(Point, CompassDirection)]
next (x, y) N grid | curr == AngleU = [((x, y + 1), E)]
                   | curr == AngleD = [((x, y - 1), W)]
                   | curr == HSplit = [((x, y - 1), W), ((x, y + 1), E)]
                   | otherwise = [((x - 1, y), N)]
                 where curr = (grid !! x) !! y
next (x, y) S grid | curr == AngleU = [((x, y - 1), W)]
                   | curr == AngleD = [((x, y + 1), E)]
                   | curr == HSplit = [((x, y - 1), W), ((x, y + 1), E)]
                   | otherwise = [((x + 1, y), S)]
                 where curr = (grid !! x) !! y
next (x, y) E grid | curr == AngleU = [((x - 1, y), N)]
                   | curr == AngleD = [((x + 1, y), S)]
                   | curr == VSplit = [((x - 1, y), N), ((x + 1, y), S)]
                   | otherwise = [((x, y + 1), E)]
                 where curr = (grid !! x) !! y
next (x, y) W grid | curr == AngleU = [((x + 1, y), S)]
                   | curr == AngleD = [((x - 1, y), N)]
                   | curr == VSplit = [((x - 1, y), N), ((x + 1, y), S)]
                   | otherwise = [((x, y - 1), W)]
                 where curr = (grid !! x) !! y


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [[Tile]]
parseInput = sepEndBy1 parseRow eol

parseRow :: (Monad m) => ParsecT Void String m [Tile]
parseRow = many parseTile

parseTile :: (Monad m) => ParsecT Void String m Tile
parseTile = (char '.' *> pure Empty) 
        <|> (char '|' *> pure VSplit)
        <|> (char '-' *> pure HSplit)
        <|> (char '/' *> pure AngleU)
        <|> (char '\\' *> pure AngleD)