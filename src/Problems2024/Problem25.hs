module Problems2024.Problem25 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Void

data Schematic = Lock [Int] | Key [Int]
    deriving (Show)

isLock :: Schematic -> Bool
isLock (Lock _) = True
isLock _ = False

isKey :: Schematic -> Bool
isKey (Key _) = True
isKey _ = False

runEasy :: FilePath -> IO String
runEasy fp = do
    schematics <- parseFile parseInput fp 
    let locks = filter isLock schematics
        keys = filter isKey schematics
    return $ show $ length $ filter id $ (map fit locks) <*> keys

runHard :: FilePath -> IO String
runHard _ = return ""

fit :: Schematic -> Schematic -> Bool
fit (Lock xs) (Key ys) =  all (<=5) $ zipWith (+) xs ys
fit _ _ = False

parseInput :: (Monad m) => ParsecT Void String m [Schematic]
parseInput = sepEndBy1 (parseLock <|> parseKey) eol

parseLock :: (Monad m) => ParsecT Void String m Schematic
parseLock = do
    _ <- string "#####\n"
    rows <- count 5 (parseLine)
    _ <- string ".....\n"
    return $ Lock $ map countLock $ map (\n -> map (\r -> r !! n) rows) [0..4]

parseKey :: (Monad m) => ParsecT Void String m Schematic
parseKey = do
    _ <- string ".....\n"
    rows <- count 5 (parseLine)
    _ <- string "#####\n"
    return $ Key $ map countKey $ map (\n -> map (\r -> r !! n) rows) [0..4]

countLock :: String -> Int
countLock s = length $ takeWhile (=='#') s

countKey :: String -> Int
countKey s = 5 - (length $ takeWhile (=='.') s)

parseLine :: (Monad m) => ParsecT Void String m String
parseLine = do 
    chars <- count 5 (char '.' <|> char '#')
    _ <- eol
    return chars