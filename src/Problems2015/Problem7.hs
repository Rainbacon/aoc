module Problems2015.Problem7 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Control.Monad.State as ST
import Data.Word
import Data.Bits
import Debug.Trace


data Gate =
    Signal Word16
  | Identifier String
  | And' Gate Gate
  | Or Gate Gate
  | Not Gate
  | LShift Gate Int
  | RShift Gate Int
  | Pass Gate
    deriving (Show)

type Wires = M.Map String Gate

runEasy :: FilePath -> IO String
runEasy fp = do
    wires <- parseFile parseInput fp
    putStrLn "What wire do you seek?"
    wire <- getLine
    return $ show $ ST.evalState (getOrCalculate wire) wires

getOrCalculate :: String -> ST.State Wires Word16
getOrCalculate wire = do
    wires <- ST.get
    case (wires M.! wire) of
        (Signal i) -> return i
        (Identifier g) -> getOrCalculate g
        gate -> do
            val <- evalGate gate
            ST.modify (M.insert wire (Signal val))
            return val

evalGate :: Gate -> ST.State Wires Word16
evalGate gate = case gate of
    (Signal i) -> return i
    (Identifier g) -> getOrCalculate g 
    (And' a b) -> do
        aVal <- evalGate a
        bVal <- evalGate b
        return $ aVal .&. bVal
    (Or a b) -> do
        aVal <- evalGate a
        bVal <- evalGate b
        return $ aVal .|. bVal
    (Not a) -> do
        aVal <- evalGate a
        return $ complement aVal
    (LShift a n) -> do 
        aVal <- evalGate a
        return $ shift aVal n
    (RShift a n) -> do 
        aVal <- evalGate a
        return $ shift aVal (n * (-1))
    (Pass a) -> evalGate a

runHard :: FilePath -> IO String
runHard fp = do
    wires <- parseFile parseInput fp
    putStrLn "What wire do you seek?"
    wire <- getLine
    let ans = ST.evalState (getOrCalculate wire) wires
    let newWires = M.insert "b" (Signal ans) wires
    return $ show $ ST.evalState (getOrCalculate wire) newWires

parseInput :: (Monad m) => ParsecT Void String m Wires
parseInput = do
    wires <- sepEndBy1 parseWire eol
    return $ M.fromList wires

parseGateInput :: (Monad m) => ParsecT Void String m Gate
parseGateInput = (some letterChar >>= return . Identifier) <|> (some digitChar >>= return . Signal . read)

parseWire :: (Monad m) => ParsecT Void String m (String, Gate)
parseWire = do
    gate <- parseGate
    string " -> "
    wire <- some letterChar
    return $ (wire, gate)

parseGate :: (Monad m) => ParsecT Void String m Gate
parseGate = (try parseAnd) <|> (try parseOr) <|> (try parseNot) <|> (try parseLShift) <|> (try parseRShift) <|> parsePass

parseAnd :: (Monad m) => ParsecT Void String m Gate
parseAnd = do
    i1 <- parseGateInput
    string " AND "
    i2 <- parseGateInput
    return $ And' i1 i2

parseOr :: (Monad m) => ParsecT Void String m Gate
parseOr = do
    i1 <- parseGateInput
    string " OR "
    i2 <- parseGateInput
    return $ Or i1 i2

parseNot :: (Monad m) => ParsecT Void String m Gate
parseNot = do
    string "NOT "
    i <- parseGateInput
    return $ Not i

parseLShift :: (Monad m) => ParsecT Void String m Gate
parseLShift = do
    i <- parseGateInput
    string " LSHIFT "
    n <- parseInt
    return $ LShift i n


parseRShift :: (Monad m) => ParsecT Void String m Gate
parseRShift = do
    i <- parseGateInput
    string " RSHIFT "
    n <- parseInt
    return $ RShift i n

parsePass :: (Monad m) => ParsecT Void String m Gate
parsePass = parseGateInput >>= return . Pass