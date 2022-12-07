module Problem1 (runEasy, runHard) where

import Data.Either
import Data.List
import Data.Void
import Text.ParserCombinators.Parsec

type Elf = Int

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- readFile fp
    let elves = parse parseInput "Problem1.hs" input
    case elves of
        Left e -> return $ error "error parsing" ++ show e
        Right p -> return (processInputEasy p)

--runHard :: FilePath -> IO String
--runHard fp = runStdoutLoggingT $ do
--    input <- parseFile parseInput fp
--    Just <$> processInputHard input

parseInput = sepEndBy1 parseInts eol
         where parseInts = many parseInt
               parseInt = do
                    let i = many1 digit
                    eol
                    return $ read $ fromRight "0" i

processInputEasy :: [Elf] -> String
processInputEasy = show . head . reverse . sort

processInputHard :: [Elf] -> String
processInputHard = show . sum . (take 3) . reverse . sort 

eol = char '\n'
