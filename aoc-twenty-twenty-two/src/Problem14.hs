module Problem14 (runEasy, runHard) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Line = (Point, Point)

runEasy :: FilePath -> IO String
runEasy _ = return ""

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m [Line]
parseInput = concat $ sepEndBy1 parseLines eol
    where parseLines = do
        points <- sepBy1 parsePoint (string " -> ")
        return $ collectPoints points

parsePoint :: (Monad m) => ParsecT Void String m Point
parsePoint = do
    y <- parseInt
    char ','
    x <- parseInt
    return (x, y)

collectPoints :: [Point] -> [Line]
collectPoints [] = []
collectPoints (x:[]) = error "only one point"
collectPoints (x:y:xs) = (x,y):(collectPoints y:xs)

determineBounds :: [Line] 
