module Problems.Y2023.Problem4 (runEasy, runHard) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace
import Utils.Parsing
import qualified Data.Map as M
import qualified Data.Maybe as Y

data Card = Card {
  cardNum :: Int,
  winning :: [Int],
  hand :: [Int]
} deriving (Show)

runEasy :: FilePath -> IO String
runEasy fp = do
              input <- parseFile parseInput fp
              return $ show $ sum $ map calcPoints input

runHard :: FilePath -> IO String
runHard fp = do
              input <- parseFile parseInput fp
              let cardCounts = M.fromList $ map (\c -> (cardNum c, 1)) input
              return $ show $ sum $ M.elems $ processCard input cardCounts

calcPoints :: Card -> Int
calcPoints (Card _ w h) | l == 0 = 0
                        | otherwise = 2 ^ (l - 1)
                      where l = length $ filter (\x -> x `elem` w) h

calcPoints' :: Card -> Int
calcPoints' (Card _ w h) = length $ filter (\x -> x `elem` w) h

processCard :: [Card] -> M.Map Int Int -> M.Map Int Int                         
processCard [] counts = counts
processCard (c:cs) counts = processCard cs newCounts
                        where copies = calcPoints' c
                              cardId = cardNum c
                              cardsToCopy = [(cardId + 1)..(cardId + copies)]
                              existingCopies = Y.fromJust $ M.lookup cardId counts 
                              newCounts = M.mapWithKey updater counts 
                              updater k v | copies == 0 = v
                                          | k `elem` cardsToCopy = v + existingCopies
                                          | otherwise = v


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [Card]
parseInput = sepEndBy1 parseCard eol

parseCard :: (Monad m) => ParsecT Void String m Card
parseCard = do
             string "Card"
             some $ char ' '
             cardId <- parseInt
             char ':'
             some spaceChar
             win <- sepEndBy1 parseInt (some $ char ' ')
             char '|'
             some $ char ' '
             h <- sepEndBy1 parseInt (some $ char ' ')
             return $ Card cardId win h