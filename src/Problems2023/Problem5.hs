module Problems2023.Problem5 where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace
import Utils.Parsing

data SeedMap = SeedMap {
    destRange :: Integer,
    startRange :: Integer,
    rangeLength :: Integer
} deriving (Show)

runEasy :: FilePath -> IO String
runEasy fp = do
    (seeds, maps) <- parseFile parseInput fp
    return $ show $ minimum $ map (seedMapper maps) seeds

runHard :: FilePath -> IO String
runHard fp = do
    (seeds, maps) <- parseFile parseInput fp
    let allSeeds = concat $ expand seeds
    return $ show $ minimum $ map (seedMapper maps) allSeeds

expand :: [Integer] -> [[Integer]]
expand [] = []
expand (x:[]) = error "odd seeds found"
expand (x:y:xs) = [x..x + y - 1]:(expand xs)

seedMapper :: [[SeedMap]] -> Integer -> Integer
seedMapper sms seed = foldl mapSeed seed sms

mapSeed :: Integer -> [SeedMap] -> Integer
mapSeed seed [] = seed
mapSeed seed ((SeedMap d s l):sms) | seed >= s && seed <= s + l - 1 = d + seed - s
                                   | otherwise = mapSeed seed sms

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m ([Integer], [[SeedMap]])
parseInput = do
    seedNums <- parseSeeds
    eol
    seedMaps <- sepEndBy parseMap eol
    return $ (seedNums, seedMaps)

parseSeeds :: (Monad m) => ParsecT Void String m [Integer]
parseSeeds = do
    string "seeds: "
    nums <- sepBy1 parseInteger (char ' ')
    eol
    return nums

parseMap :: (Monad m) => ParsecT Void String m [SeedMap]
parseMap = do
    some $ (letterChar <|> char '-')
    string " map:"
    eol
    ranges <- sepEndBy1 parseRange eol
    return ranges

parseRange :: (Monad m) => ParsecT Void String m SeedMap
parseRange = do
    dest <- parseInteger
    char ' '
    source <- parseInteger
    char ' '
    len <- parseInteger
    return $ SeedMap dest source len