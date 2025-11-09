module Problems2015.Problem7 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Maybe as Y
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
    return $ show $ wireMemo wire wires

wireMemo :: String -> Wires -> Int16
wireMemo wire wires = (M.map evalWire wires) M.! wire
    where evalWire' (Signal i) _ = i
          evalWire' (Identifier s) ws = wireMemo s ws 
          evalWire gate = trace (show gate) $ case gate of
            (And' a b) -> (evalWire' a wires) .&. (evalWire' b wires)
            (Or a b) -> (evalWire' a wires) .|. (evalWire' b wires)
            (Not a) -> 65535 `xor` (evalWire' a wires)
            (LShift a n) -> shift (evalWire' a wires) n
            (RShift a n) -> shift (evalWire' a wires) (n * (-1))
            (Pass a) -> evalWire' a wires

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