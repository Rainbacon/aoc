module Problems2025.Problem9 (runEasy, runHard) where

import Utils.Parsing
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

runEasy :: FilePath -> IO String
runEasy fp = do
    points <- parseFile (sepEndBy1 parsePoint eol) fp
    return $ show $ head $ reverse $ sort $ map area points <*> points

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = xLen * yLen
    where xLen = 1 + abs (x1 - x2)
          yLen = 1 + abs (y1 - y2)

runHard :: FilePath -> IO String
runHard _ = return ""

parsePoint :: (Monad m) => ParsecT Void String m Point
parsePoint = do
    x <- parseInt
    char ','
    y <- parseInt
    return (x, y)