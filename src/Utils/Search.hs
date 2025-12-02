module Utils.Search (
    bfs
)
where

import qualified Data.Set as S
import qualified Control.Monad.State as ST
import qualified Queue as Q
import qualified Data.Maybe as Y
import Debug.Trace

data BfsState a = BfsState {
    seen :: S.Set a
  , queue :: Q.Queue (Int, a)
  , goal :: a
}

instance (Show a) => Show (BfsState a) where
    show (BfsState s q g) = "{\n  seen: " ++ show s ++ ",\n  queue: " ++ show q ++ ",\n  goal: " ++ show "\n}"

bfs :: (Eq a, Ord a, Show a) => a -> a -> (a -> [a]) -> Int
bfs start goal findNeighbors = ST.evalState (bfs' findNeighbors) initial
    where initial = BfsState S.empty (Q.singleton (0, start)) goal

bfs' :: (Eq a, Ord a, Show a) => (a -> [a]) -> ST.State (BfsState a) Int
bfs' findNeighbors = do
    state <- ST.get
    case queue state of
        Q.Empty -> return (-1)
        Q.Full (i, node) q -> do
            if node == (goal state)
            then return i
            else do 
                let neighbors = findNeighbors node
                ST.modify ((addSeen node) . (enqueueAll neighbors (i + 1)))
                bfs' findNeighbors

addSeen :: (Ord a) => a -> BfsState a -> BfsState a
addSeen a (BfsState s q g) = BfsState (S.insert a s) q g

enqueueAll :: [a] -> Int -> BfsState a -> BfsState a
enqueueAll xs i (BfsState s q g) = BfsState s updated g
    where updated = snd $ Y.fromJust $ Q.dequeue $ foldl (\q' x -> Q.enqueue (i, x) q') q xs