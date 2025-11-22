module Problems2015.Problem16 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Maybe as Y

data Sue = Sue {
    number :: Int 
  , attrs :: (M.Map String Int) }


tickerTape = M.fromList [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)]

runEasy :: FilePath -> IO String
runEasy fp = do
    sues <- parseFile (sepEndBy1 parseSue eol) fp
    return $ show $ map number $ filter (attrMatch tickerTape) sues

attrMatch :: M.Map String Int -> Sue -> Bool
attrMatch tape (Sue _ as) = M.intersection tape as == as

runHard :: FilePath -> IO String
runHard fp = do
    sues <- parseFile (sepEndBy1 parseSue eol) fp
    return $ show $ map number $ filter (attrMatch' tickerTape) sues

attrMatch' :: M.Map String Int -> Sue -> Bool
attrMatch' tape (Sue _ as) = all id matches
    where matches = M.mapWithKey match as
          match k v = case k of
            "cats" -> v > (Y.fromJust $ M.lookup k tape)
            "trees" -> v > (Y.fromJust $ M.lookup k tape)
            "poweranians" -> v < (Y.fromJust $ M.lookup k tape)
            "goldfish" -> v < (Y.fromJust $ M.lookup k tape)
            _ -> v == (Y.fromJust $ M.lookup k tape)

parseSue :: (Monad m) => ParsecT Void String m Sue
parseSue = do
    string "Sue "
    num <- parseInt
    string ": "
    attrs <- sepBy1 parseAttr (string ", ")
    return $ Sue num (M.fromList attrs)

parseAttr :: (Monad m) => ParsecT Void String m (String, Int)
parseAttr = do
    attr <- some letterChar
    string ": "
    val <- parseInt
    return (attr, val)