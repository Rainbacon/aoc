module Problems2015.Problem13 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Debug.Trace

type Pairing = (String, String)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ head $ reverse $ sort $ map (score input) $ genPerms $ M.keys input

score :: M.Map Pairing Int -> [Pairing] -> Int
score mappings ps | valid newPs = trace (show newPs) $ sum $ map score' newPs
                  | otherwise = trace ("It's zero") $ 0
    where newPs = (snd lastP, fst (head ps)):ps
          lastP = head $ reverse ps
          score' p@(a, b) = (mappings M.! p) + (mappings M.! (b, a))

genPerms :: [Pairing] -> [[Pairing]]
genPerms = (filter valid) . permutations . S.elems . S.fromList

valid :: [Pairing] -> Bool
valid [] = True
valid ((a, b):[]) = a /= b
valid (x:y:xs) | fst x == snd x = False
               | snd x == fst y = valid (y:xs)
               | otherwise = False

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m (M.Map Pairing Int)
parseInput = sepEndBy1 parsePairing eol >>= return . M.fromList

parsePairing :: (Monad m) => ParsecT Void String m (Pairing, Int)
parsePairing = do
    name1 <- some letterChar
    string " would "
    sign <- (string "gain " >> return 1) <|> (string "lose " >> return (-1))
    magnitude <- parseInt
    string " happiness units by sitting next to "
    name2 <- some letterChar
    char '.'
    return ((name1, name2), sign * magnitude)