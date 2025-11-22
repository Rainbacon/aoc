module Problems2015.Problem6 (runEasy, runHard) where

import Utils.Parsing
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M

data Command = Toggle | On | Off

type Instruction = (Command, Point, Point)

base :: M.Map Point Bool
base = M.fromList [((x, y), False) | x <- [0..999], y <- [0..999]]

runEasy :: FilePath -> IO String
runEasy fp = do
    instructions <- parseFile parseInput fp 
    return $ show $ M.size $ M.filter id $ foldl applyInstruction base instructions

applyInstruction :: M.Map Point Bool -> Instruction -> M.Map Point Bool
applyInstruction existing (c, (x1, y1), (x2, y2)) = let change = M.fromList [((x, y), False) | x <- [x1..x2], y <- [y1..y2]]
    in case c of
        Toggle -> M.unionWith (\a _ -> not a) existing change
        On -> M.unionWith (\_ _ -> True) existing change
        Off -> M.unionWith (\_ _ -> False) existing change

base' :: M.Map Point Int
base' = M.fromList [((x, y), 0) | x <- [0..999], y <- [0..999]]

runHard :: FilePath -> IO String
runHard fp = do
    instructions <- parseFile parseInput fp
    return $ show $ M.foldr (+) 0 $ foldl applyInstruction' base' instructions

applyInstruction' :: M.Map Point Int -> Instruction -> M.Map Point Int
applyInstruction' existing (c, (x1, y1), (x2, y2)) = let change = M.fromList [((x, y), 0) | x <- [x1..x2], y <- [y1..y2]]
    in case c of
        Toggle -> M.unionWith (\a _ -> a + 2) existing change
        On -> M.unionWith (\a _ -> a + 1) existing change
        Off -> M.unionWith (\a _ -> max (a - 1) 0) existing change

parseInput :: (Monad m) => ParsecT Void String m [Instruction]
parseInput = sepEndBy1 parseInstruction eol

parseInstruction :: (Monad m) => ParsecT Void String m Instruction
parseInstruction = do
    c <- parseCommand
    p1 <- parsePoint
    string " through "
    p2 <- parsePoint
    return $ (c, p1, p2)

parseCommand :: (Monad m) => ParsecT Void String m Command
parseCommand = parseToggle <|> parseOn <|> parseOff

parseToggle :: (Monad m) => ParsecT Void String m Command
parseToggle = do
    string "toggle "
    return $ Toggle

parseOn :: (Monad m) => ParsecT Void String m Command
parseOn = do
    string "turn on "
    return $ On

parseOff :: (Monad m) => ParsecT Void String m Command
parseOff = do
    string "turn off "
    return $ Off

parsePoint :: (Monad m) => ParsecT Void String m Point
parsePoint = do
    x <- parseInt
    char ','
    y <- parseInt
    return $ (x, y)