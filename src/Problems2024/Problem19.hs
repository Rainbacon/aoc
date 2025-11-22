module Problems2024.Problem19 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Maybe as Y
import qualified Data.Map as M
import qualified Control.Monad.State as ST

runEasy :: FilePath -> IO String
runEasy fp = do
    (patterns, desired) <- parseFile parseInput fp
    return $ show $ length $ filter id $ map (isPossible patterns) desired

isPossible :: [String] -> String -> Bool
isPossible _ [] = True
isPossible patterns design = any isPossible' patterns
    where isPossible' pattern = isSuffixOf pattern design && isPossible patterns (take ((length design) - (length pattern)) design)

runHard :: FilePath -> IO String
runHard fp = do
    (patterns, desired) <- parseFile parseInput fp
    return $ show $ sum $ ST.evalState (mapM (genDesigns patterns) desired) M.empty

genDesigns :: [String] -> String -> ST.State (M.Map String Int) Int
genDesigns _ [] = return 1
genDesigns patterns design = do
    cache <- ST.get
    if M.member design cache
    then return $ cache M.! design
    else do
        let possible = filter (\p -> p `isSuffixOf` design) patterns 
            prefixes = map (\p -> take ((length design) - (length p)) design) possible
        n <- ST.foldM (\acc p -> genDesigns patterns p >>= return . (+acc)) 0 prefixes
        ST.modify (M.insert design n)
        return n

parseInput :: (Monad m) => ParsecT Void String m ([String], [String])
parseInput = do
    patterns <- sepBy1 (some letterChar) (string ", ") 
    _ <- eol
    _ <- eol
    desired <- sepEndBy1 (some letterChar) eol
    return (patterns, desired)