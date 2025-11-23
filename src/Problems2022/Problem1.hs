module Problems2022.Problem1 (runEasy, runHard) where

import Data.Either
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

type Elf = [Int]

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return (processInputEasy input)

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    return  (processInputHard input)

parseInput :: (Monad m) => ParsecT Void String m [Elf]
parseInput = sepEndBy1 parseIntLines eol
         where parseIntLines = some parseIntLine
               parseIntLine = do
                    i <- read <$> some digitChar
                    eol
                    return i

processInputEasy :: [Elf] -> String
processInputEasy = show . head . reverse . sort . (map sum)

processInputHard = show . sum . (take 3) . reverse . sort . (map sum)
