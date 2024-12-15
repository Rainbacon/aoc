module Problems.Y2024.Problem11 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Debug.Trace
import qualified Data.Map as M
import qualified Control.Monad.State as ST

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ length $ foldl (\acc _-> concatMap replace acc) input [0..24] 

replace :: Int -> [Int]
replace n | n == 0 = [1]
          | evenDigits = split'
          | otherwise = [n * 2024]
        where numDigits = length $ show n
              evenDigits = numDigits `mod` 2 == 0
              split' = map read $ [take (numDigits `div` 2) (show n), drop (numDigits `div` 2) (show n)]

replace' :: M.Map Int Int -> M.Map Int Int
replace' xs = foldl (\acc (n, x) -> M.alter (setOrIncr x) n acc) M.empty xs'
          where xs' = concatMap replace'' $ M.assocs xs
                replace'' (k, v) = map (\k' -> (k',v)) (replace k)


runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let initMap = foldl (\acc n -> M.alter (setOrIncr 1) n acc) M.empty input
    return $ show $ M.foldl (+) 0 $ foldl (\acc _ -> replace' acc) initMap [0..74] 

setOrIncr :: Int -> Maybe Int -> Maybe Int
setOrIncr n Nothing = Just n
setOrIncr n x = fmap (+n) x

parseInput :: (Monad m) => ParsecT Void String m [Int]
parseInput = sepEndBy1 parseInt (char ' ')