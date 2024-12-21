module Problems2024.Problem21 (runEasy, runHard) where

import Utils
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST
import Debug.Trace

type Keypad = M.Map Point Char
type ReverseKeypad = M.Map Char Point

runEasy :: FilePath -> IO String
runEasy fp = do
    codes <- parseFast lines fp
    let nKey = (numericKeypad, reverseKeypad numericKeypad)
        dKey = (directionalKeypad, reverseKeypad directionalKeypad)
        keyPads = map (\(ks, p) -> generateSequence ks p) [(nKey, (3,2)), (dKey, (0, 2)), (dKey, (0, 2))]
        evaluate code = foldl (\acc f -> trace ("Mapped " ++ acc ++ " to " ++ reverse (f acc)) $ reverse $ f acc) code keyPads
    return $ show $ sum $ map (\code -> complexity code (evaluate code)) codes

complexity :: String -> String -> Int
complexity code dirs = trace ("Complexity of " ++ show code ++ " has length " ++ show dirs) $ length dirs * numCode
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
generateSequence (k, r) p (x:xs) = trace ("Found sequence " ++ s ++ " to move from " ++ show y ++ " to " ++ show x ) $ (generateSequence (k, r) p' xs) ++ s
    where p' = r M.! x
          s = 'A' : (ST.evalState (findPath p' k) ([(p, "")], S.empty))
          y = k M.! p

findPath :: Point -> Keypad -> ST.State ([(Point, String)], S.Set Point) String
findPath target keypad = do
    (queue, seen) <- ST.get
    let (p, path) = head queue
        rest = tail queue
    if p == target
    then return path 
    else do
        let toQueue = filter (\(q, _) -> S.notMember q seen) $ map (fmap (:path)) $ neighbors p keypad
            newQueue = foldl insertQueue rest toQueue
        ST.put (newQueue, S.insert p seen)
        findPath target keypad

insertQueue :: [(Point, String)] -> (Point, String) -> [(Point, String)]
insertQueue [] item = [item]
insertQueue (h@(p, s):ps) item@(p', s') | p == p' = h:ps
                                        | length s' < length s = item:h:(filter (\(q, _) -> q /= p') ps)
                                        | otherwise = h:(insertQueue ps item)

neighbors :: Point -> Keypad -> [(Point, Char)]
neighbors (x, y) keypad = filter (\(p, _) -> M.member p keypad) candidates
    where candidates = [((x - 1, y), '^'), ((x + 1, y), 'v'), ((x, y - 1), '<'), ((x, y + 1), '>')]

runHard :: FilePath -> IO String
runHard _ = return ""
