module Problems2025.Problem10 (runEasy, runHard) where

import Utils.Parsing
import Utils.Search
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Bits
import Debug.Trace

data Machine = Machine {
    lights :: Int
  , buttons :: [Int]
  , joltages :: [Int]
} deriving (Show)

runEasy :: FilePath -> IO String
runEasy fp = do
    machines <- parseFile (sepEndBy1 parseMachine eol) fp
    let val = map findTarget machines
    return $ show $ val !! 62

findTarget :: Machine -> Int
findTarget (Machine ls bs _) = bfs 0 ls (pushButtons bs)

pushButtons :: [Int] -> Int -> [Int]
pushButtons bs ls = map (xor ls) bs

runHard :: FilePath -> IO String
runHard _ = return ""

parseMachine :: (Monad m) => ParsecT Void String m Machine
parseMachine = do
    ls <- char '[' >> some parseLight <* string "] "
    bs <- sepEndBy1 parseButton (char ' ')
    js <- char '{' >> (sepBy1 parseInt (char ',')) <* char '}'
    return $ Machine (constructLight ls) bs js

constructLight :: [Bool] -> Int
constructLight ls = fromBits bitsSet
    where indexed = zip [0..] ls
          bitsSet = map fst $ filter (id . snd) indexed 

fromBits :: [Int] -> Int
fromBits bs = foldl setBit 0 bs

parseLight :: (Monad m) => ParsecT Void String m Bool
parseLight = (char '.' >> pure False) <|> (char '#' >> pure True)

parseButton :: (Monad m) => ParsecT Void String m Int
parseButton = do
    char '('
    indices <- sepBy1 parseInt (char ',')
    char ')'
    return $ fromBits indices