module Problems.Y2022.Problem6  where

import qualified Data.List as L
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

type Signal = (String, String, String)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile (parseInput 4) fp
    return (processInput input)

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile (parseInput 14) fp
    return (processInput input)

processInput :: Signal -> String
processInput (n, s, r) = show $ (length n) + (length s)

parseInput :: (Monad m) => Int -> ParsecT Void String m Signal
parseInput n = do
              noise <- manyTill letterChar (try $ lookAhead $ parseStart n)
              start <- parseStart n
              rest <- some letterChar
              return (noise, start, rest)

parseStart :: (Monad m) => Int -> ParsecT Void String m String
parseStart n = do
              letters <- count n letterChar
              let allDiff = L.nub letters == letters
              if allDiff then return letters else fail "some letters are the same"
