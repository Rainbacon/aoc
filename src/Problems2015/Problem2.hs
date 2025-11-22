module Problems2015.Problem2 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

type Box = (Int, Int, Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    boxes <- parseFile parseInput fp
    return $ show $ sum $ map area boxes

area :: Box -> Int
area (l, w, h) = 2*l*w + 2*l*h + 2*w*h + (minimum [l*w,l*h,w*h])

runHard :: FilePath -> IO String
runHard fp = do 
    boxes <- parseFile parseInput fp
    return $ show $ sum $ map ribbon boxes

ribbon :: Box -> Int
ribbon (l, w, h) = l*w*h + (minimum $ [(2*)] <*> [l+w,l+h,w+h])

parseInput :: (Monad m) => ParsecT Void String m [Box]
parseInput = sepEndBy1 parseBox eol

parseBox :: (Monad m) => ParsecT Void String m Box
parseBox = do
    l <- parseInt
    char 'x'
    w <- parseInt
    char 'x'
    h <- parseInt
    return $ (l, w, h)