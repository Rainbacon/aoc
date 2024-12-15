module Problems.Y2024.Problem13 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Control.Monad.State as ST

data ClawMachine = ClawMachine {
    buttonA :: (Int, Int)
  , buttonB :: (Int, Int)
  , prize :: (Int, Int)
}

type Queue = [(Int, (Int, Int))]

runEasy :: FilePath -> IO String
runEasy fp = do
    machines <- parseFile parseInput fp
    return $ show $ sum $ map (\m -> ST.evalState (solveMachine m) [(0, (0, 0))]) machines

solveMachine :: ClawMachine -> ST.State (Queue) Int
solveMachine machine@(ClawMachine (x_a, y_a) (x_b, y_b) (x_p, y_p)) = do
    queue <- ST.get
    case queue of
        [] -> return 0
        ((cost, (x, y)):xs) -> do
            if x == x_p && y == y_p
            then return cost 
            else do
                ST.put $ filter (\(_, (x', y')) -> x' <= x_p && y' <= y_p) $ sort $ [(cost + 3, (x + x_a, y + y_a)), (cost + 1, (x + x_b, y + y_b))] ++ xs
                solveMachine machine

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m [ClawMachine]
parseInput = sepEndBy1 parseMachine eol

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