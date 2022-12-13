module Problem11 where

import qualified Control.Monad.State as ST
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Item = Integer
data Monkey = Monkey [Item] (Integer -> Integer) (Integer -> Bool) (M.Map Bool Integer)
type Monkies = M.Map Integer Monkey
type ItemsInspected = M.Map Integer Integer
type ProgramState = (Monkies, ItemsInspected, Integer -> Integer)

instance Show Monkey where
    show (Monkey items _ _ conditions) = "items: " ++ show items ++ " | conditions: " ++ show conditions ++ "\n"

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let initialState = (input, M.fromList (zip (M.keys input) (repeat 0)), (\x -> x `div` 3))
    let (_, itemsInspected, _) = ST.execState (chaseMonkies 20) initialState
    return $ show $ product $ take 2 $ reverse $ L.sort $ M.elems itemsInspected

runHard :: FilePath -> IO String
runHard fp = do 
    input <- parseFile parseInput fp
    let initialState = (input, M.fromList (zip (M.keys input) (repeat 0)), id)
    let (_, itemsInspected, _) = ST.execState (chaseMonkies 10000) initialState
    return $ show $ product $ take 2 $ reverse $ L.sort $ M.elems itemsInspected

chaseMonkies :: Integer -> ST.State ProgramState ()
chaseMonkies 0 = return ()
chaseMonkies n = do
    (monkies, _, _) <- ST.get
    mapM_ runMonkey (M.keys monkies)
    chaseMonkies (n - 1)

runMonkey :: Integer -> ST.State ProgramState ()
runMonkey n = do
    (monkies, _, _) <- ST.get
    let m@(Monkey items op test conditions) = Y.fromJust $ M.lookup n monkies
    mapM_ (processItem n m) items
    (monkies, itemsInspected, wc) <- ST.get
    ST.put (M.adjust clearItems n monkies, itemsInspected, wc)

processItem :: Integer -> Monkey -> Item -> ST.State ProgramState ()
processItem n (Monkey _ op test conditions) item = do
    (monkies, itemsInspected, worryCond) <- ST.get
    let newItem = worryCond (op item)
    let cond = test newItem
    let newMonkey = Y.fromJust $ M.lookup cond conditions
    let newMonkies = M.adjust (addItem newItem) newMonkey monkies
    ST.put (newMonkies, M.adjust (+1) n itemsInspected, worryCond)
    
addItem :: Item -> Monkey -> Monkey
addItem item (Monkey items op test cond) = Monkey (items ++ [item]) op test cond

clearItems :: Monkey -> Monkey
clearItems (Monkey _ op test cond) = Monkey [] op test cond

-- #--------------#
-- | Parsing Code |
-- #--------------#
parseInput :: (Monad m) => ParsecT Void String m Monkies
parseInput = do
              ms <- sepEndBy1 parseMonkey eol
              return $ M.fromList ms

parseMonkey :: (Monad m) => ParsecT Void String m (Integer, Monkey)
parseMonkey = do
    string "Monkey "
    num <- parseInteger
    char ':'
    eol
    string "  Starting items: "
    items <- sepBy1 parseInteger (string ", ")
    eol
    string "  Operation: new = old "
    op <- parsePlus <|> parseTimes
    string "  Test: divisible by "
    test <- parseInteger
    eol
    string "    If true: throw to monkey "
    trueCase <- parseInteger
    eol
    string "    If false: throw to monkey "
    falseCase <- parseInteger
    eol
    let cases = M.fromList [(True, trueCase), (False, falseCase)]
    return (num, Monkey items op (\x -> x `mod` test == 0) cases) 

parseInteger :: (Monad m) => ParsecT Void String m Integer
parseInteger = read <$> some digitChar

parsePlus :: (Monad m) => ParsecT Void String m (Integer -> Integer)
parsePlus = do
             string "+ "
             val <- (some digitChar) <|> (string "old")
             eol
             case val of
                "old" -> return (\x -> x + x)
                _ -> return (+ (read val))

parseTimes :: (Monad m) => ParsecT Void String m (Integer -> Integer)
parseTimes = do
             string "* "
             val <- (some digitChar) <|> (string "old")
             eol
             case val of
                "old" -> return (\x -> x * x)
                _ -> return (* (read val))
