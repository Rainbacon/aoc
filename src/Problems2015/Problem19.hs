module Problems2015.Problem19 (runEasy, runHard) where

import Utils.Parsing
import Utils.Search
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
    (replacements, base) <- parseFile parseInput fp
    let mapping = foldl (\m (k, v) -> M.insertWith (++) k [v] m) M.empty replacements
    return $ show $ S.size $ foldl1 S.union $ map (\(k, v) -> generateMolecules base k v) $ M.assocs mapping

generateMolecules :: String -> String -> [String] -> S.Set String
generateMolecules base find replacements = S.fromList mols
    where indices = findIndices find base
          replacements' = map (replaceAtIndex base) replacements
          mols = replacements' <*> indices

replaceAtIndex :: String -> String -> (Int, Int) -> String
replaceAtIndex base target (index, l) = h ++ target ++ t
    where h = take index base
          t = drop (index + l) base

findIndices :: String -> String -> [(Int, Int)]
findIndices target str = findIndices' 0 target str
    where findIndices' _ _ [] = []
          findIndices' n t s | t `isPrefixOf` s = (n, length t):(findIndices' (n + length t) t (drop (length t) s))
                             | otherwise = findIndices' (n + 1) t (tail s)

runHard :: FilePath -> IO String
runHard fp = do
    (replacements, target) <- parseFile parseInput fp
    let mapping = foldl (\m (k, v) -> M.insertWith (++) v [k] m) M.empty replacements
    return $ show $ bfs target "e" (generateMolecules' mapping)

generateMolecules' :: M.Map String [String] -> String -> [String]
generateMolecules' mapping base = S.toList $ foldl1 S.union $ map (\(k, v) -> generateMolecules base k v) $ M.assocs mapping

parseInput :: (Monad m) => ParsecT Void String m ([(String, String)], String)
parseInput = do
    replacements <- sepEndBy1 parseReplacement eol
    eol
    target <- some letterChar
    return (replacements, target)

parseReplacement :: (Monad m) => ParsecT Void String m (String, String)
parseReplacement = do
    find <- some letterChar
    string " => "
    replace <- some letterChar
    return (find, replace)