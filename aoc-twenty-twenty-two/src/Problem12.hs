module Problem12 (runEasy, runHard) where

import qualified Data.Char as C
import qualified Data.Map as M
import Data.Void
import Utils

type InputType = (Position, Position, M.Map Position Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- readFile fp
    let data = parseInput $ lines input

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: [String] -> InputType
parseInput xs = let positions = mapPosition xs
                    (start, end) = foldl parseCell (Nothing, Nothing) positions
                in (Y.fromJust start, Y.fromJust end, M.fromList $ map toHeight positions)

parseCell :: (Maybe Position, Maybe Position) -> (Position, Char) -> (Maybe Position, Maybe Position)
parseCell (s, e) (p, 'S') = (Just p, e)
parseCell (s, e) (p, 'E') = (s, Just p)
parseCell xs _ = xs

toHeight :: Char -> Int
toHeight 'S' = 1
toHeight 'E' = 26
toHeight c = C.ord c - 96
