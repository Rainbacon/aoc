module Problems2015.Problem3 (runEasy, runHard) where

import qualified Data.Set as S
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Direction = U | D | L | R

runEasy :: FilePath -> IO String
runEasy fp = do
    directions <- parseFile parseInput fp 
    let locations = scanl move (0, 0) directions
    return $ show $ S.size $ S.fromList locations

move :: Point -> Direction -> Point
move (x, y) d = case d of
    U -> (x, y + 1)
    D -> (x, y - 1)
    L -> (x - 1, y)
    R -> (x + 1, y)

runHard :: FilePath -> IO String
runHard fp = do
    directions <- parseFile parseInput fp 
    let (santa, robot) = splitDirections True directions 
    let santaL = S.fromList $ scanl move (0, 0) santa
    let robotL = S.fromList $ scanl move (0, 0) robot
    return $ show $ S.size $ S.union santaL robotL

splitDirections :: Bool -> [Direction] -> ([Direction], [Direction])
splitDirections _ [] = ([], [])
splitDirections s (d:ds) | s == True = (d:santa, robot)
                         | otherwise = (santa, d:robot)
                       where (santa, robot) = splitDirections (not s) ds

parseInput :: (Monad m) => ParsecT Void String m [Direction]
parseInput = manyTill (up <|> down <|> left <|> right) eol 

up :: (Monad m) => ParsecT Void String m Direction
up = do
    char '^' 
    return $ U
down :: (Monad m) => ParsecT Void String m Direction
down = do
    char 'v' 
    return $ D
left :: (Monad m) => ParsecT Void String m Direction
left = do
    char '<' 
    return $ L
right :: (Monad m) => ParsecT Void String m Direction
right = do
    char '>' 
    return $ R