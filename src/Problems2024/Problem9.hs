module Problems2024.Problem9 (runEasy, runHard) where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad.State as ST
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils
import Debug.Trace
import Data.Char

type Disk = M.Map Int (Maybe Int)
data Block = FBlock Int Int | EBlock Int
    deriving (Show)

isFile :: Block -> Bool
isFile (FBlock _ _) = True
isFile _ = False

type HardDisk = M.Map Int Block

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast (map (read . (:[]))) fp
    let disk = M.fromList $ zip [0..] (expand input 0 True)
        finalIndex = maximum $ M.keys disk
        remappedDisk = execState (remapMem 0 finalIndex) disk
    return $ show $ sum $ M.elems $ M.mapWithKey checksum remappedDisk

runHard :: FilePath -> IO String
runHard fp = do
    blocks <- parseFast (map (read . (:[]))) fp
    let blocks' = zipper 0 True blocks
        hDisk = M.fromList $ zip [0,10..] blocks'
        blocksToRemap = reverse $ M.keys $ M.filter isFile hDisk 
        remappedDisk = execState (mapM_ remapMem' blocksToRemap) hDisk
    return $ show $ sum $ zipWith checksum [0..] $ concatMap expand' $ M.elems remappedDisk

zipper :: Int -> Bool -> [Int] -> [Block]
zipper _ _ [] = []
zipper n True (x:xs) = (FBlock x n) : (zipper (n+1) False xs)
zipper n False (x:xs) = (EBlock x) : (zipper n True xs)

checksum :: Int -> Maybe Int -> Int
checksum b (Just a) = a * b
checksum b Nothing = 0

expand :: [Int] -> Int -> Bool -> [Maybe Int]
expand [] _ _ = []
expand (x:xs) n True = replicate x (Just n) ++ expand xs (n + 1) False
expand (x:xs) n False = replicate x Nothing ++ expand xs n True

remapMem :: Int -> Int -> ST.State Disk ()
remapMem low high | low >= high = return ()
                  | otherwise = do
                        disk <- ST.get
                        let targetCell = Y.fromJust $ M.lookup low disk
                            sourceCell = Y.fromJust $ M.lookup high disk
                            (newTarget, newSource) = swap targetCell sourceCell
                            newHigh = if Y.isNothing targetCell then high - 1 else high
                            newLow = if Y.isNothing sourceCell && Y.isNothing targetCell then low else low + 1
                        ST.put $ M.adjust (\_ -> newTarget) low $ M.adjust (\_ -> newSource) high disk
                        remapMem newLow newHigh


swap :: Maybe Int -> Maybe Int -> (Maybe Int, Maybe Int)
swap Nothing a = (a, Nothing)
swap a b = (a, b)

printDisk :: Disk -> String
printDisk disk = concat $ map print' $ M.assocs disk
             where print' (_, Nothing) = "."
                   print' (_, (Just a)) = show a

remapMem' :: Int -> ST.State HardDisk ()
remapMem' n = do
    hDisk <- ST.get
    let block = Y.fromJust $ M.lookup n hDisk
        freeSpace = findFree block $ M.filterWithKey (\k _ -> k < n) hDisk
        newDisk = swap' n block freeSpace hDisk 
    ST.put newDisk

swap' :: Int -> Block -> Maybe Int -> HardDisk -> HardDisk
swap' _ _ Nothing hDisk = hDisk
swap' i block (Just n) hDisk = foldl ins hDisk newBlocks 
                           where targetBlock = Y.fromJust $ M.lookup n hDisk
                                 newBlocks = zip [n..] (splitBlock targetBlock block) ++ [(i, nowFree)]
                                 nowFree = freeBlock block
                                 ins acc (k, b) = M.alter (\_ -> Just b) k acc


findFree :: Block -> HardDisk -> Maybe Int
findFree block hDisk = fmap fst $ M.lookupMin $ M.filter (canFree block) hDisk
                   where canFree a (FBlock _ _) = False
                         canFree x@(FBlock n _) (EBlock m) = m >= n
                         canFree (EBlock _) _ = False

printBlock :: Block -> String
printBlock (EBlock n) = replicate n '.'
printBlock (FBlock n i) = replicate n (intToDigit i)

freeHead :: [Block] -> [Block]
freeHead [] = []
freeHead (x:xs) = freeBlock x : xs
                          
freeBlock :: Block -> Block
freeBlock (FBlock x _) = EBlock x
freeBlock a = a

splitBlock :: Block -> Block -> [Block]
splitBlock (EBlock x) a@(FBlock y _) | x == y = [a]
                                     | otherwise = [a, (EBlock (x - y))]
splitBlock _ _ = error "Can't split"

expand' :: Block -> [Maybe Int]
expand' (FBlock s i) = replicate s (Just i)
expand' (EBlock s) = replicate s Nothing