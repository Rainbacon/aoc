module Problems2015.Problem14 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

type Reindeer = (String, Int, Int, Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    deer <- parseFile parseInput fp
    putStrLn "How long are the reindeer flying?"
    t <- read <$> getLine
    return $ unlines $ map (format t) $ reverse $ sortKey (distance t) deer

sortKey :: Ord b => (a -> b) -> [a] -> [a]
sortKey f = sortBy (\a b -> compare (f a) (f b))

format :: Int -> Reindeer -> String
format t r@(n, _, _, _) = n ++ " traveled " ++ show (distance t r) ++ " km"

distance :: Int -> Reindeer -> Int
distance t (_, s, d, r) = full * s * d + partial
    where full = t `div` (d + r)
          rem = t `mod` (d + r)
          partial = case compare rem d of
            LT -> rem * s
            otherwise -> d * s

runHard :: FilePath -> IO String
runHard fp = do
    deer <- parseFile parseInput fp
    putStrLn "How long are the reindeer flying?"
    t <- read <$> getLine
    return $ show $ maximum $ scores $ take t $ zipN $ map (distance' 0) deer

scores :: [[Int]] -> [Int]
scores = foldl1 (zipWith (+)) 

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

zipN :: [[Int]] -> [[Int]]
zipN ds = points:(zipN rest)
    where slice = map head ds
          rest = map tail ds
          m = maximum slice
          points = map (boolToInt . (== m)) slice

distance' :: Int -> Reindeer -> [Int]
distance' i deer = base ++ (distance' newI deer)
    where base = (iterateDeer i deer)
          newI = head $ reverse base

iterateDeer :: Int -> Reindeer -> [Int]
iterateDeer n r@(_, s, d, rest) | d == 0 = replicate rest n
                                | otherwise = tot:(iterateDeer tot (sub r))
    where tot = (s + n)
          sub (a, b, c, e) = (a, b, c - 1, e)

parseInput :: (Monad m) => ParsecT Void String m [Reindeer]
parseInput = sepEndBy1 parseReindeer eol

parseReindeer :: (Monad m) => ParsecT Void String m Reindeer
parseReindeer = do
    n <- some letterChar
    string " can fly "
    s <- parseInt
    string " km/s for "
    d <- parseInt
    string " seconds, but then must rest for "
    r <- parseInt
    string " seconds."
    return (n, s, d, r)