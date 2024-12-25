module Problems2024.Problem21 (runEasy, runHard) where

import Utils
import Data.Tuple
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST
import Data.MemoTrie
import Debug.Trace

type Keypad = M.Map Point Char
type ReverseKeypad = M.Map Char Point

runEasy :: FilePath -> IO String
runEasy fp = do
    codes <- parseFast lines fp
    let nKey = (numericKeypad, reverseKeypad numericKeypad)
        dKey = (directionalKeypad, reverseKeypad directionalKeypad)
        keyPads = map (\(ks, p) -> generateSequence ks p) [(nKey, (3,2)), (dKey, (0, 2)), (dKey, (0, 2))]
        evaluate code = foldl (\acc f -> f acc) code keyPads
    return $ show $ sum $ map (\code -> complexity code (evaluate code)) codes

complexity :: String -> String -> Int
complexity code dirs = length dirs * numCode
    where numCode = read (take (length code - 1) code)

reverseKeypad :: Keypad -> ReverseKeypad
reverseKeypad k = M.fromList $ map swap $ M.assocs k

numericKeypad :: Keypad
numericKeypad = M.fromList [
    ((0,0), '7'), ((0,1), '8'), ((0,2), '9'),
    ((1,0), '4'), ((1,1), '5'), ((1,2), '6'),
    ((2,0), '1'), ((2,1), '2'), ((2,2), '3'),
    ((3,1), '0'), ((3,2), 'A')]

directionalKeypad :: Keypad
directionalKeypad = M.fromList [
    ((0,1), '^'), ((0, 2), 'A'),
    ((1,0), '<'), ((1,1), 'v'), ((1,2), '>')]

generateSequence :: (Keypad, ReverseKeypad) -> Point -> String -> String
generateSequence _ _ [] = []
generateSequence (k, r) p@(x, y) (t:xs) = (s ++ "A") ++ (generateSequence (k, r) p' xs)
    where p'@(x', y') = r M.! t
          v = if x > x' then take (x - x') (repeat '^') else take (x' - x) (repeat 'v')
          h = if y > y' then take (y - y') (repeat '<') else take (y' - y) (repeat '>')
          prios = prioritizeMoves v h
          s = fixErrors prios p k

prioritizeMoves :: String -> String -> String
prioritizeMoves [] h = h
prioritizeMoves v [] = v
prioritizeMoves v h@('<':hs) = h ++ v
prioritizeMoves v h@('>':hs) = v ++ h

fixErrors :: String -> Point -> Keypad -> String
fixErrors [] _ _ = []
fixErrors ('<':rest) p@(x, y) k | M.member (x, y - (length lefts)) k = lefts ++ (fixErrors (dropWhile (=='<') rest) (x, y - (length lefts)) k)
                                | otherwise = fixErrors newQueue p k
    where newQueue = verts ++ lefts ++ rest''
          lefts = '<':(takeWhile (=='<') rest)
          rest' = dropWhile (=='<') rest
          verts = takeWhile (== (head rest')) rest'
          rest'' = dropWhile (== (head rest')) rest'
fixErrors ('^':rest) p@(x, y) k | M.member (x - (length ups), y) k = ups ++ (fixErrors (dropWhile (=='^') rest) (x - (length ups), y) k)
                                | otherwise = fixErrors newQueue p k
    where newQueue = horizontals ++ ups ++ rest''
          ups = '^':(takeWhile (=='^') rest)
          rest' = dropWhile (=='^') rest
          horizontals = takeWhile (== (head rest')) rest'
          rest'' = dropWhile (== (head rest')) rest'
fixErrors ('v':rest) p@(x, y) k | M.member (x + (length downs), y) k = downs ++ (fixErrors (dropWhile (=='v') rest) (x + length(downs), y) k)
                                | otherwise = fixErrors newQueue p k
    where newQueue = horizontals ++ downs ++ rest''
          downs = 'v':(takeWhile (=='v') rest)
          rest' = dropWhile (=='v') rest
          horizontals = takeWhile (== (head rest')) rest'
          rest'' = dropWhile (== (head rest')) rest'
fixErrors ('>':rest) p@(x, y) k = '>':(fixErrors rest (x, y + 1) k)

          
runHard :: FilePath -> IO String
runHard fp = do
    codes <- parseFast lines fp
    return $ show $ sum $ map (\code -> complexity' code (genSequence (pairs ('A':code)) 0 26)) codes

complexity' :: String -> Int -> Int
complexity' code l = l * numCode 
    where numCode = read (take (length code - 1) code)

genSequence :: [(Char, Char)] -> Int -> Int -> Int
genSequence ps d md = sum $ map (\(a, b) -> memoSequence b a d md) ps

memoSequence :: Char -> Char -> Int -> Int -> Int
memoSequence = memo4 countSequence

memo4 = mup memo3

countSequence :: Char -> Char -> Int -> Int -> Int
countSequence target source depth maxDepth | depth == maxDepth = 1
                                           | otherwise = genSequence (pairs s) (depth + 1) maxDepth
    where p = r M.! source
          s = (generateSequence' (k, r) source target)
          (k, r) = if depth == 0 then (numericKeypad, reverseKeypad numericKeypad) else (directionalKeypad, reverseKeypad directionalKeypad)

generateSequence' :: (Keypad, ReverseKeypad) -> Char -> Char -> String
generateSequence' (k, r) source target = "A" ++ s ++ "A"
    where p@(x, y) = r M.! source
          p'@(x', y') = r M.! target
          v = if x > x' then take (x - x') (repeat '^') else take (x' - x) (repeat 'v')
          h = if y > y' then take (y - y') (repeat '<') else take (y' - y) (repeat '>')
          prios = prioritizeMoves v h
          s = fixErrors prios p k

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:[]) = []
pairs (x:y:xs) = ((x,y):(pairs (y:xs)))
          