module Problems2015.Problem11 (runEasy, runHard) where

import Utils.Parsing
import Data.List
import qualified Data.Maybe as Y

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast id fp
    return $ reverse $ head $ filter isValid $ scanl (\a _ -> increment a) (reverse input) [0..]

increment :: String -> String
increment [] = []
increment ('z':xs) = 'a':(increment xs)
increment (x:xs) = n:xs
    where n = next $ (1+) $ idx x

isValid :: String -> Bool
isValid s =  hasRun s && hasDoubles s 0
    where hasRun s' | length s' < 3 = False
                    | i1 - i2 == 1 && i2 - i3 == 1 = True
                    | otherwise = hasRun $ tail s'
                  where i1 = idx (s' !! 0)
                        i2 = idx (s' !! 1)
                        i3 = idx (s' !! 2)
          hasDoubles s' n | length s' < 2 = False
                          | (s' !! 0) == (s' !! 1) && n == 1 = True
                          | (s' !! 0) == (s' !! 1) = hasDoubles (drop 2 s') (n + 1)
                          | otherwise = hasDoubles (drop 1 s') n

idx :: Char -> Int
idx c = Y.fromJust $ elemIndex c "abcdefghjkmnpqrstuvwxyz"

next :: Int -> Char
next i = "abcdefghjkmnpqrstuvwxyz" !! i

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast id fp
    return $ reverse $ head $ tail $ filter isValid $ scanl (\a _ -> increment a) (reverse input) [0..]
