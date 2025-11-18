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
score mappings ps = sum $ map score' newPs
    where score' p@(a, b) = (mappings M.! p) + (mappings M.! (b, a))
          newPs = (snd lastP, fst $ head ps):ps
          lastP = head $ reverse ps

genPerms :: [Pairing] -> [[Pairing]]
genPerms = (map toPairings) . permutations . S.elems . S.fromList . concat . (map (\(a,b) -> [a,b]))
    where toPairings [] = []
          toPairings (x:[]) = []
          toPairings (x:y:xs) = (x,y):(toPairings (y:xs))

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let input' = addMe input
    return $ show $ head $ reverse $ sort $ map (score input') $ genPerms $ M.keys input'

addMe :: M.Map Pairing Int -> M.Map Pairing Int
addMe mapping = M.union mapping myMapping
    where people = S.elems $ S.fromList $ concat $ (map (\(a,b) -> [a,b])) $ M.keys mapping
          myMapping = M.fromList $ concat $ map (\person -> [(("Me", person), 0), ((person, "Me"), 0)]) people

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