module Problems2022.Problem17 (runEasy, runHard) where

import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils
import Debug.Trace

data Dir = L | R
data Stone = Minus | Plus | Ell | Bar | Box
type Tower = S.Set Point
type Block = S.Set Point
type ProgramState = ([Dir], Tower)

runEasy :: FilePath -> IO String
runEasy fp = do
  directions <- parseFile parseInput fp
  let stones = cycle [Minus, Plus, Ell, Bar, Box]
  let initialState = (cycle directions, S.fromList $ zip (repeat 0) [0..8])
  let (_, tower) = ST.execState (runStones (take 2022 stones)) initialState
  -- putStrLn $ showTower (startStone Minus tower) tower
  return $ show $ maximum $ S.map fst tower

runHard :: FilePath -> IO String
runHard fp = do
  directions <- parseFile parseInput fp
  let stones = cycle [Minus, Plus, Ell, Bar, Box]
  let initialState = (cycle directions, S.fromList $ zip (repeat 0) [0..8])
  let cycleLength = lcm 5 (length directions)
  let numCycles = 1000000000000 `div` cycleLength
  let extra = 1000000000000 `mod` cycleLength
  let (_, tower) = ST.execState (runStones (take cycleLength stones)) initialState
  let towerHeight = maximum $ S.map fst tower
  let (_, extraTower) = ST.execState (runStones (take extra stones)) initialState
  let extraHeight = maximum $ S.map fst extraTower
  return $ show $ (numCycles * towerHeight) + extraHeight
  


showTower :: Block -> Tower -> String
showTower block tower = unlines $ reverse $ ("        ":) $ map mapLine [0..top]
                    where top = maximum $ S.map fst block
                          mapLine row = zipWith showTile (repeat row) [0..8]
                          showTile _ 0 = '|'
                          showTile _ 8 = '|'
                          showTile 0 _ = '_'
                          showTile x y | S.member (x, y) block = '@'
                                       | S.member (x, y) tower = '#'
                                       | otherwise = '.'

runStones :: [Stone] -> ST.State ProgramState ()
runStones stones = mapM_ runStone stones

runStone :: Stone -> ST.State ProgramState ()
runStone stone = do
  (_, tower) <- ST.get
  let block = startStone stone tower 
  runBlock block

runBlock :: Block -> ST.State ProgramState ()
runBlock block = do
  (ds, tower) <- ST.get
  let hBlock = processBlow block (head ds) tower 
  let dBlock = processFall hBlock tower
  let newTower = S.union tower hBlock
  case dBlock of
    (Just b) -> do 
                 ST.put (tail ds, tower)
                 runBlock b
    Nothing -> ST.put (tail ds, newTower)


processBlow :: Block -> Dir -> Tower -> Block
processBlow block d tower | valid = newBlock
                          | otherwise = block
                        where newBlock = move block d 
                              left = minimum $ S.map snd newBlock
                              right = maximum $ S.map snd newBlock 
                              intersect = S.intersection tower newBlock
                              valid = left > 0 && right < 8 && length intersect == 0

processFall :: Block -> Tower -> Maybe Block
processFall block tower | valid = Just newBlock
                        | otherwise = Nothing
                      where newBlock = S.map (\(x, y) -> (x - 1, y)) block
                            valid = length (S.intersection tower newBlock) == 0

move :: Block -> Dir -> Block
move block L = S.map (\(x, y) -> (x, y - 1)) block
move block R = S.map (\(x, y) -> (x, y + 1)) block

startStone :: Stone -> Tower -> Block
startStone stone tower = let topOfTower = maximum $ S.map fst tower 
                             base = topOfTower + 4
                         in case stone of
                            Minus -> S.fromList $ zip (repeat $ base) [3..6]
                            Plus -> S.fromList [(base + 1, 3), (base + 1, 4), (base + 1, 5), (base + 2, 4), (base, 4)]
                            Ell -> S.fromList [(base, 3), (base, 4), (base, 5), (base + 1, 5), (base + 2, 5)]
                            Bar -> S.fromList $ zip [base..base+3] (repeat 3) 
                            Box -> S.fromList [(base, 3), (base, 4), (base + 1, 3), (base + 1, 4)]

parseInput :: (Monad m) => ParsecT Void String m [Dir]
parseInput = many $ (char '>' *> pure R) <|> (char '<' *> pure L)