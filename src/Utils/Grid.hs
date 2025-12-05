module Utils.Grid (
    Grid
  , GridShow
  , toChar
  , constructGrid
  , constructGridSkipEmpty
  , displayGrid
  , Point
  , neighbors
) where

import Utils (Point, mapPos)
import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as Y

type Grid a = M.Map Point a

constructGrid :: [[a]] -> Grid a
constructGrid = M.fromList . mapPos

constructGridSkipEmpty :: [[Maybe a]] -> Grid a
constructGridSkipEmpty = (M.map Y.fromJust) . (M.filter Y.isJust) . M.fromList . mapPos

neighbors :: Point -> [Point]
neighbors p@(a, b) = filter (/= p) [(a+x, b+y) | x <- [-1,0,1], y <- [-1,0,1]]

class GridShow a where
    toChar :: a -> Char

displayGrid :: (GridShow a) => Grid a -> String
displayGrid grid = '\n':(unlines rows)
    where rows = map (map toChar) rows'
          rows' = map (map snd) $ groupBy fsts $ M.assocs grid
          fsts ((a, _), _) ((b, _), _) = a == b