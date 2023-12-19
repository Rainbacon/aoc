module Problems2023.Problem11 where

import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils
import Debug.Trace

type Space = S.Set Point

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let maxX = maximum $ S.map fst input
    let minX = minimum $ S.map fst input
    let maxY = maximum $ S.map snd input
    let minY = minimum $ S.map snd input
    let rowsToExpand = filter (shouldExpandRow input) [minX..maxX]
    let colsToExpand = filter (shouldExpandCol input) [minY..maxY]
    let galaxies = S.toList $ expand input 1 rowsToExpand colsToExpand
    return $ show $ (`div` 2) $ sum $ (map dist galaxies) <*> galaxies

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let maxX = maximum $ S.map fst input
    let minX = minimum $ S.map fst input
    let maxY = maximum $ S.map snd input
    let minY = minimum $ S.map snd input
    let rowsToExpand = filter (shouldExpandRow input) [minX..maxX]
    let colsToExpand = filter (shouldExpandCol input) [minY..maxY]
    let galaxies = S.toList $ expand input 999999 rowsToExpand colsToExpand
    return $ show $ (`div` 2) $ sum $ (map dist galaxies) <*> galaxies

expand :: Space -> Int -> [Int] -> [Int] -> Space
expand space factor rows cols = S.map (calcExpanse factor rows cols) space

calcExpanse :: Int -> [Int] -> [Int] -> Point -> Point
calcExpanse factor rows cols (x, y) = (x + (factor * length rows'), y + (factor * length cols'))
                           where rows' = filter (< x) rows
                                 cols' = filter (< y) cols

shouldExpandRow :: Space -> Int -> Bool
shouldExpandRow space row = (length $ S.filter ((== row) . fst) space) == 0

shouldExpandCol :: Space -> Int -> Bool
shouldExpandCol space col = (length $ S.filter ((== col) . snd) space) == 0

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m Space
parseInput = do
    lines <- sepEndBy1 parseLine eol
    let rowed = concat $ zipWith buildPoints [0..] lines
    return $ S.fromList $ map fst $ filter (Y.isJust . snd) rowed

buildPoints :: Int -> [(Int, a)] -> [(Point, a)]
buildPoints row xs = map (\(col, val) -> ((row, col), val)) xs

parseLine :: (Monad m) => ParsecT Void String m [(Int, Maybe Bool)]
parseLine = do
    items <- many $ parseEmpty <|> parseGalaxy
    return $ zip [0..] items

parseEmpty :: (Monad m) => ParsecT Void String m (Maybe Bool)
parseEmpty = char '.' *> pure Nothing

parseGalaxy :: (Monad m) => ParsecT Void String m (Maybe Bool)
parseGalaxy = char '#' *> pure (Just True)