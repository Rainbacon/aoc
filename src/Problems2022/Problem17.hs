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

data Dir = L | R
data Stone = Minus | Plus | Ell | Bar | Box
type Tower = S.Set Point
type Block = S.Set Point
type ProgramState = ([Dir], Tower)

runEasy :: FilePath -> IO String
runEasy fp = do
  directions <- parseFile parseInput fp
  let stones = cycle [Minus, Plus, Ell, Bar, Box]
  let initialState = (cycle directions, S.fromList $ zip [0..8] (repeat 0))
  let (_, tower) = ST.execState (runStones (take 2022 stones)) initialState
  return $ show $ maximum $ S.map snd tower

runHard :: FilePath -> IO String
runHard _ = return ""

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
                              left = minimum $ S.map fst newBlock
                              right = maximum $ S.map fst newBlock 
                              intersect = S.intersection tower newBlock
                              valid = left > 0 && right< 8 && length intersect == 0

processFall :: Block -> Tower -> Maybe Block
processFall block tower | valid = Just newBlock
                        | otherwise = Nothing
                      where newBlock = S.map (\(x, y) -> (x, y - 1)) block
                            valid = length (S.intersection tower newBlock) == 0

move :: Block -> Dir -> Block
move block L = S.map (\(x, y) -> (x - 1, y)) block
move block R = S.map (\(x, y) -> (x + 1, y)) block

startStone :: Stone -> Tower -> Block
startStone stone tower = let topOfTower = maximum $ S.map snd tower 
                             base = topOfTower + 4
                         in case stone of
                            Minus -> S.fromList $ zip [2..5] (repeat $ base)
                            Plus -> S.fromList [(2, base + 1), (3, base + 1), (4, base + 1), (3, base + 2), (3, base)]
                            Ell -> S.fromList [(2, base), (3, base), (4, base), (4, base + 1), (4, base + 2)]
                            Bar -> S.fromList $ zip (repeat 2) [base..base+3]
                            Box -> S.fromList [(2, base), (3, base), (2, base + 1), (3, base + 1)]

parseInput :: (Monad m) => ParsecT Void String m [Dir]
parseInput = many $ (char '>' *> pure R) <|> (char '<' *> pure L)