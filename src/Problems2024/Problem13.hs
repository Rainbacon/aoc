module Problems2024.Problem13 (runEasy, runHard) where

import Utils.Parsing
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Matrix as M
import qualified Control.Monad.State as ST
import qualified Data.Set as S
import Debug.Trace

data ClawMachine = ClawMachine (Int, Int) (Int, Int) (Int, Int) 
data BigClawMachine = BigClawMachine (Integer, Integer) (Integer, Integer) (Integer, Integer)

runEasy :: FilePath -> IO String
runEasy fp = do
    machines <- parseFile parseInputEasy fp
    let initialPosition = ((0, 0), 0)
        initialState = ([initialPosition], S.empty)
    return $ show $ sum $ map (\m -> ST.evalState (solveMachine m) initialState) machines

solveMachine :: ClawMachine -> ST.State (PrioQueue, Visited) Int
solveMachine machine@(ClawMachine (x_a, y_a) (x_b, y_b) (x_p, y_p)) = do
    (queue, seen) <- ST.get
    case queue of
        [] -> return 0
        (((x, y), cost):xs) -> do
            if x == x_p && y == y_p
            then return cost 
            else do
                let newPaths = [((x + x_a, y + y_a), cost + 3), ((x + x_b, y + y_b), cost + 1)]
                    unseenPaths = filter (\(z@(x', y'), cost) -> x' <= x_p && y' <= y_p && S.notMember z seen) newPaths
                ST.put $ (insertPaths unseenPaths xs, S.insert (x, y) seen)
                solveMachine machine

runHard :: FilePath -> IO String
runHard fp = do
    machines <- parseFile parseInputHard fp
    return $ show $ sum $ map (solve . createMatrix) machines

createMatrix :: BigClawMachine -> M.Matrix Integer
createMatrix (BigClawMachine (x_a, y_a) (x_b, y_b) (x_p, y_p)) = M.fromLists [[x_a, x_b, x_p], [y_a, y_b, y_p]]

solve :: M.Matrix Integer -> Integer
solve m | a_det `mod` denom /= 0 = 0
        | b_det `mod` denom /= 0 = 0
        | otherwise = 3 * (a_det `div` denom) + (b_det `div` denom)
      where subm m = M.submatrix 1 2 1 2 m
            denom = M.detLaplace $ subm m
            a_det = M.detLaplace $ subm $ M.switchCols 1 3 m 
            b_det = M.detLaplace $ subm $ M.switchCols 2 3 m 

parseInputEasy :: (Monad m) => ParsecT Void String m [ClawMachine]
parseInputEasy = sepEndBy1 parseMachine eol

parseMachine :: (Monad m) => ParsecT Void String m ClawMachine
parseMachine = do
    _ <- string "Button A: X+"
    x_a <- parseInt
    _ <- string ", Y+"
    y_a <- parseInt
    _ <- eol
    _ <- string "Button B: X+"
    x_b <- parseInt
    _ <- string ", Y+"
    y_b <- parseInt
    _ <- eol
    _ <- string "Prize: X="
    x_p <- parseInt
    _ <- string ", Y="
    y_p <- parseInt
    _ <- eol
    return $ ClawMachine (x_a, y_a) (x_b, y_b) (x_p, y_p)

parseInputHard :: (Monad m) => ParsecT Void String m [BigClawMachine]
parseInputHard = sepEndBy1 parseMachine' eol

parseMachine' :: (Monad m) => ParsecT Void String m BigClawMachine
parseMachine' = do
    _ <- string "Button A: X+"
    x_a <- parseInteger
    _ <- string ", Y+"
    y_a <- parseInteger
    _ <- eol
    _ <- string "Button B: X+"
    x_b <- parseInteger
    _ <- string ", Y+"
    y_b <- parseInteger
    _ <- eol
    _ <- string "Prize: X="
    x_p <- parseInteger
    _ <- string ", Y="
    y_p <- parseInteger
    _ <- eol
    return $ BigClawMachine (x_a, y_a) (x_b, y_b) (10000000000000 + x_p, 10000000000000 + y_p)