module Utils.Grid (
    Grid
  , GridShow
  , toChar
  , constructGrid
  , displayGrid
) where

import Utils (Point, mapPos)
import Data.List
import qualified Data.Map as M

type Grid a = M.Map Point a

constructGrid :: [[a]] -> Grid a
constructGrid = M.fromList . mapPos

class GridShow a where
    toChar :: a -> Char

displayGrid :: (GridShow a) => Grid a -> String
displayGrid grid = '\n':(unlines rows)
    where rows = map (map toChar) rows'
          rows' = map (map snd) $ groupBy fsts $ M.assocs grid
          fsts ((a, _), _) ((b, _), _) = a == b