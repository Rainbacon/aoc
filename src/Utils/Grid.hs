module Utils.Grid (
    Grid
  , GridShow
  , toChar
  , constructGrid
  , displayGrid
  , Point
  , neighbors
) where

import Utils (Point, mapPos)
import Data.List
import qualified Data.Map as M

type Grid a = M.Map Point a

constructGrid :: [[a]] -> Grid a
constructGrid = M.fromList . mapPos

neighbors :: Point -> [Point]
neighbors p@(a, b) = filter (/= p) [(a+x, b+y) | x <- [-1,0,1], y <- [-1,0,1]]

class GridShow a where
    toChar :: a -> Char

displayGrid :: (GridShow a) => Grid a -> String
displayGrid grid = '\n':(unlines rows)
    where rows = map (map toChar) rows'
          rows' = map (map snd) $ groupBy fsts $ M.assocs grid
          fsts ((a, _), _) ((b, _), _) = a == b