module Problem3 (run) where

import qualified Data.Set as S
import qualified Data.Map as M

type Sack = (S.Set Char, S.Set Char)
type Elf = S.Set Char

runEasy :: String -> String
runEasy =  show . sum . (map (findPriority . findIncorrect)) . (map getSack) . lines

priorities :: M.Map Char Int
priorities = M.fromList $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

getSack :: String -> Sack
getSack xs = let numItems = length xs `div` 2
                 first = take numItems xs
                 second = drop numItems xs
             in (S.fromList first, S.fromList second)

findIncorrect :: Sack -> Char
findIncorrect (f, s) = head $ S.elems $ S.intersection f s

findPriority :: Char -> Int
findPriority c = case M.lookup c priorities of
                    (Just i) -> i
                    Nothing -> 0

run :: String -> String
run = show . sum . (map (findPriority . findBadge)) . groupElves . (map S.fromList) . lines

groupElves :: [Elf] -> [[Elf]]
groupElves [] = []
groupElves es = (take 3 es):(groupElves (drop 3 es))

findBadge :: [Elf] -> Char
findBadge = head . S.elems . (foldl1 S.intersection)
