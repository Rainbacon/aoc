module Problems2015.Problem7 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import Data.Int
import Data.Bits

data Gate =
    Signal Int16
  | And' String String
  | Or String String
  | Not String
  | LShift String Int
  | RShift String Int
    deriving (Show)

type Wires = M.Map String Gate

runEasy :: FilePath -> IO String
runEasy fp = do
    wires <- parseFile parseInput fp
    putStrLn "What wire do you seek?"
    wire <- getLine
    putStrLn $ show wires
    return $ show $ evalWire wire wires

evalWire :: String -> Wires -> Int16
evalWire wire wires = let gate = M.lookup wire wires
    in case gate of
        Nothing -> error $ "Looked up missign gate " ++ show wire
        (Just (Signal n)) -> n
        (Just (And' a b)) -> (evalWire a wires) .&. (evalWire b wires)
        (Just (Or a b)) -> (evalWire a wires) .|. (evalWire b wires)
        (Just (Not a)) -> 65535 `xor` (evalWire a wires)
        (Just (LShift a n)) -> shift (evalWire a wires) n
        (Just (RShift a n)) -> shift (evalWire a wires) (n * (-1))

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m Wires
parseInput = do
    wires <- sepEndBy1 parseWire eol
    return $ M.fromList wires

parseWire :: (Monad m) => ParsecT Void String m (String, Gate)
parseWire = do
    gate <- parseGate
    string " -> "
    wire <- many letterChar
    return $ (wire, gate)

parseGate :: (Monad m) => ParsecT Void String m Gate
parseGate = parseSignal <|> parseAnd <|> parseOr <|> parseNot <|> parseLShift <|> parseRShift

parseSignal :: (Monad m) => ParsecT Void String m Gate
parseSignal = do
    sig <- many digitChar
    return $ Signal (read sig)

parseAnd :: (Monad m) => ParsecT Void String m Gate
parseAnd = do
    i1 <- many letterChar
    string " AND "
    i2 <- many letterChar
    return $ And' i1 i2

parseOr :: (Monad m) => ParsecT Void String m Gate
parseOr = do
    i1 <- many letterChar
    string " OR "
    i2 <- many letterChar
    return $ Or i1 i2

parseNot :: (Monad m) => ParsecT Void String m Gate
parseNot = do
    string "NOT "
    i <- many letterChar
    return $ Not i

parseLShift :: (Monad m) => ParsecT Void String m Gate
parseLShift = do
    i <- many letterChar
    string " LSHIFT "
    n <- parseInt
    return $ LShift i n


parseRShift :: (Monad m) => ParsecT Void String m Gate
parseRShift = do
    i <- many letterChar
    string " RSHIFT "
    n <- parseInt
    return $ RShift i n