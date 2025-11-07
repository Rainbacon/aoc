module Problems2015.Problem7 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import Data.Int
import Data.Bits
import Debug.Trace

data GateInput = Signal Int16 | Identifier String
    deriving (Show)

data Gate =
    And' GateInput GateInput
  | Or GateInput GateInput
  | Not GateInput
  | LShift GateInput Int
  | RShift GateInput Int
  | Pass GateInput
    deriving (Show)

type Wires = M.Map String Gate

runEasy :: FilePath -> IO String
runEasy fp = do
    wires <- parseFile parseInput fp
    putStrLn "What wire do you seek?"
    wire <- getLine
    return $ show $ evalWire wire wires

evalWire :: String -> Wires -> Int16
evalWire wire wires = let gate = trace ("wire " ++ wire ++ " " ++ (show $ M.lookup wire wires)) $ M.lookup wire wires
    in case gate of
        Nothing -> error $ "Looked up missign gate " ++ show wire
        (Just (And' a b)) -> (evalWire' a wires) .&. (evalWire' b wires)
        (Just (Or a b)) -> (evalWire' a wires) .|. (evalWire' b wires)
        (Just (Not a)) -> 65535 `xor` (evalWire' a wires)
        (Just (LShift a n)) -> shift (evalWire' a wires) n
        (Just (RShift a n)) -> shift (evalWire' a wires) (n * (-1))
        (Just (Pass a)) -> evalWire' a wires

evalWire' :: GateInput -> Wires -> Int16
evalWire' (Signal i) _ = i
evalWire' (Identifier s) ws = evalWire s ws 

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m Wires
parseInput = do
    wires <- sepEndBy1 parseWire eol
    return $ M.fromList wires

parseGateInput :: (Monad m) => ParsecT Void String m GateInput
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