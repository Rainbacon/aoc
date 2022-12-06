module Problem4 (run) where

import qualified Data.Set as S

type Pair = (S.Set Int, S.Set Int)

runEasy :: String -> String
runEasy = show . length . (filter (== True)) . (map (determineOverlap . parse)) . lines

parse :: String -> Pair
parse s = let first = takeWhile (/= ',') s
              second = tail $ dropWhile (/= ',') s
          in (parseSingle first, parseSingle second)

parseSingle :: String -> S.Set Int
parseSingle s = let first = takeWhile (/= '-') s
                    second = tail $ dropWhile (/= '-') s
                in S.fromList [(read first)..(read second)]

determineOverlap :: Pair -> Bool
determineOverlap (f, s) = (S.isSubsetOf f s) || (S.isSubsetOf s f)

determinePartialOverlap :: Pair -> Bool
determinePartialOverlap (f, s) = not $ S.disjoint f s

run :: String -> String
run = show . length . (filter (== True)) . (map (determinePartialOverlap. parse)) . lines
