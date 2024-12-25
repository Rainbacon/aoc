module Problems2024.Problem23 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

runEasy :: FilePath -> IO String
runEasy fp = do
    connections <- parseFile parseInput fp
    let mappings = foldl buildMapping M.empty connections
        tComps = filter (\k -> "t" `isPrefixOf` k) $ M.keys mappings
        triples = concatMap (findTriples mappings) tComps
    return $ show $ S.size $ S.fromList triples

buildMapping :: M.Map String [String] -> (String, String) -> M.Map String [String]
buildMapping mapping (a, b) = M.unionWith (++) mapping $ M.fromList [(a,[b]), (b,[a])]

findTriples :: M.Map String [String] -> String -> [(String, String, String)]
findTriples mapping key = concatMap buildTriple processed
    where connections = mapping M.! key
          processed = map expand connections
          expand conn = (conn, filter (\c -> c `elem` connections) $ mapping M.! conn)
          buildTriple (c, cs) = [buildTriple' (sort [key, c, x]) | x <- cs]
          buildTriple' (x:y:z:[]) = (x, y, z)

runHard :: FilePath -> IO String
runHard fp = do
    connections <- parseFile parseInput fp
    let mappings = foldl buildMapping M.empty connections
        triples = map (\(a,b,c) -> [a,b,c]) $ S.toList $ S.fromList $ concatMap (findTriples mappings) $ M.keys mappings
    return $ intercalate "," $ sort $ grow mappings triples

grow :: M.Map String [String] -> [[String]] -> [String]
grow mappings cliques | length next > 0 = grow mappings next
                      | otherwise = head cliques
    where next = S.elems $ S.fromList $ map sort $ concatMap (expandGroup mappings) cliques
          n = length (head next)

expandGroup :: M.Map String [String] -> [String] -> [[String]]
expandGroup mappings xs@(x:_) = [winner:xs | winner <- winners]
    where candidates = mappings M.! x
          winners = filter (\c -> all (\y -> y `elem` (mappings M.! c)) xs) candidates

parseInput :: (Monad m) => ParsecT Void String m [(String, String)]
parseInput = sepEndBy1 parseConnection eol

parseConnection :: (Monad m) => ParsecT Void String m (String, String)
parseConnection = do
    a <- some letterChar
    _ <- char '-'
    b <- some letterChar
    return (a, b)