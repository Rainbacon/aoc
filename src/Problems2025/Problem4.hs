module Problems2025.Problem4 (runEasy, runHard) where

import Utils.Parsing
import Utils.Grid
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Maybe as Y

type WrappingPaper = Grid Char

runEasy :: FilePath -> IO String
runEasy fp = do
    grid <- parseFile parseGrid fp
    return $ show $ M.size $ M.filterWithKey (fewNeighbors grid) grid

fewNeighbors :: WrappingPaper -> Point -> Char -> Bool
fewNeighbors grid p _ = length realNeighbors < 4
    where realNeighbors = filter Y.isJust $ map (\p -> M.lookup p grid) $ neighbors p

runHard :: FilePath -> IO String
runHard fp = do
    grid <- parseFile parseGrid fp
    return $ show $ iterativeRemoval grid

iterativeRemoval :: WrappingPaper -> Int
iterativeRemoval grid | length accessible == 0 = 0
                      | otherwise = length accessible + iterativeRemoval removed
    where accessible = M.keys $ M.filterWithKey (fewNeighbors grid) grid
          removed = removeAll accessible grid

removeAll :: [Point] -> WrappingPaper -> WrappingPaper
removeAll ps grid = foldl (flip M.delete) grid ps

parseGrid :: (Monad m) => ParsecT Void String m WrappingPaper
parseGrid = do
    rows <- sepEndBy1 (parseRow) eol
    return $ constructGridSkipEmpty rows

parseRow :: (Monad m) => ParsecT Void String m [Maybe Char]
parseRow = some $ (char '.' >> pure Nothing) <|> (char '@' >> pure (Just '@'))