module Problems.Y2023.Problem25 (runEasy, runHard) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Control.Monad.State as ST
import Control.Monad.Random
import Utils.Parsing
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

type Wire = (String, String)
type Wires = M.Map String [String]
type Seen = S.Set String

runEasy :: FilePath -> IO String
runEasy fp = do
  input <- parseFile parseInput fp
  let graph = foldl graphInsert M.empty input
  samples <- replicateM 100 $ evalRandIO $ sampleGraph graph
  putStrLn "Generated samples"
  let frequencies = M.assocs $ buildFrequencies $ concat samples
  putStrLn "Generated frequencies"
  let sortedFrequencies = sortBy (\(_, a) (_, b) -> compare b a) frequencies
  putStrLn $ show $ take 3 sortedFrequencies
  let top10 = map fst $ take 10 sortedFrequencies
  putStrLn "Found top 10 frequencies"
  let perms = permutations top10 
  putStrLn "Generated permutations"
  putStrLn $ show $ length perms
  let tests = nub $ map (take 3) perms
  putStrLn $ "There are " ++ show (length tests) ++ " tests to run"
  let tested = filter (testBisection . (removeWires graph)) tests
  let solution = head tested
  let snipped = removeWires graph solution
  let (subgraph, _, _) = ST.execState bfs (S.empty, snipped, [head $ M.keys snipped])
  let subgraphSize = S.size subgraph
  let inputSize = M.size graph
  return $ show $ subgraphSize * (inputSize - subgraphSize)

runHard :: FilePath -> IO String
runHard _ = return "Last day! No part 2!"

buildFrequencies :: [Wire] -> M.Map Wire Int
buildFrequencies [] = M.empty
buildFrequencies (x:xs) = M.insertWith (+) x 1 $ buildFrequencies xs

graphInsert :: M.Map String [String] -> Wire -> Wires
graphInsert graph (a, b) = M.insertWith (++) a [b] $ M.insertWith (++) b [a] graph

removeWires :: Wires -> [Wire] -> Wires
removeWires wires [] = wires
removeWires wires ((x, y):xs) = removeWires removedWires xs 
                       where removedWires = M.adjust (remove x) y $ M.adjust (remove y) x wires 
                             remove el lst = lst \\ [el]

testBisection :: Wires -> Bool
testBisection wires = let (seen, _, _) = ST.execState bfs (S.empty, wires, [head $ M.keys wires])
                      in S.size seen /= M.size wires

bfs :: ST.State (Seen, Wires, [String]) ()
bfs = do
  (seen, wires, queue) <- ST.get
  case queue of
    [] -> return ()
    (x:xs) -> do
      let connections = Y.fromJust $ M.lookup x wires
      let unseen = filter (\el -> not $ S.member el seen) connections
      ST.put (S.insert x seen, wires, queue ++ unseen)
      bfs

dfs :: [String] -> String -> ST.State (Seen, Wires) (Maybe [String])
dfs path@(current:xs) target =
  if current == target
  then return $ Just path 
  else do
    (seen, wires) <- ST.get
    ST.put (S.insert current seen, wires)
    let connections = filter (\el -> not $ S.member el seen) $ Y.fromJust $ M.lookup current wires
    continuation <- mapM (\conn -> dfs (conn:path) target) connections
    let rest = filter Y.isJust continuation
    if length rest == 0
    then return Nothing
    else do
      return $ head rest

sampleGraph :: (RandomGen g) => Wires -> Rand g [Wire]
sampleGraph wires = do
  let nodes = M.keys wires
  startIndex <- getRandomR (0, length nodes - 1)
  endIndex <- getRandomR (0, length nodes - 1)
  let path = ST.evalState (dfs [(nodes !! startIndex)] (nodes !! endIndex)) (S.empty, wires)
  case path of
    Nothing -> return []
    (Just ps) -> return $ toWires $ reverse ps

toWires :: [String] -> [Wire]
toWires [] = []
toWires (x:[]) = []
toWires (x:y:xs) = (x, y):(toWires (y:xs))

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [Wire]
parseInput = concat <$> sepEndBy1 parseWire eol

parseWire :: (Monad m) => ParsecT Void String m [Wire]
parseWire = do
  leftEnd <- some letterChar
  string ": "
  rightEnds <- sepEndBy1 (some letterChar) (char ' ')
  return $ map ((,) leftEnd) rightEnds