module Problem11 where

import qualified Control.Monad.State as ST
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Item = Int
data Monkey = Monkey [Item] (Int -> Int) Int (M.Map Bool Int)
type Monkies = M.Map Int Monkey
type ItemsInspected = M.Map Int Int
type ProgramState = (Monkies, ItemsInspected, Int, Int -> Int)

instance Show Monkey where
    show (Monkey items _ _ conditions) = "items: " ++ show items ++ " | conditions: " ++ show conditions ++ "\n"

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let supermod = foldr modProd 1 $ M.elems input
    let initialState = (input, M.fromList (zip (M.keys input) (repeat 0)), supermod, \x -> x `div` 3)
    let (_, itemsInspected, _, _) = ST.execState (chaseMonkies 20) initialState
    return $ show $ product $ take 2 $ reverse $ L.sort $ M.elems itemsInspected

runHard :: FilePath -> IO String
runHard fp = do 
    input <- parseFile parseInput fp
    let supermod = foldr modProd 1 $ M.elems input
    let initialState = (input, M.fromList (zip (M.keys input) (repeat 0)), supermod, id)
    let (_, itemsInspected, _, _) = ST.execState (chaseMonkies 10000) initialState
    return $ show $ product $ take 2 $ reverse $ L.sort $ M.elems itemsInspected

modProd :: Monkey -> Int -> Int
modProd (Monkey _ _ test _) i = i * test 

chaseMonkies :: Int -> ST.State ProgramState ()
chaseMonkies 0 = return ()
chaseMonkies n = do
    (monkies, _, _, _) <- ST.get
    mapM_ runMonkey (M.keys monkies)
    chaseMonkies (n - 1)

runMonkey :: Int -> ST.State ProgramState ()
runMonkey n = do
    (monkies, _, _, _) <- ST.get
    let m@(Monkey items op test conditions) = Y.fromJust $ M.lookup n monkies
    mapM_ (processItem n m) items
    (monkies, itemsInspected, supermod, wc) <- ST.get
    ST.put (M.adjust clearItems n monkies, itemsInspected, supermod, wc)

processItem :: Int -> Monkey -> Item -> ST.State ProgramState ()
processItem n (Monkey _ op test conditions) item = do
    (monkies, itemsInspected, supermod, wc) <- ST.get
    let newItem = (wc (op item)) `mod` supermod
    let cond = (newItem `mod` test) == 0
    let newMonkey = Y.fromJust $ M.lookup cond conditions
    let newMonkies = M.adjust (addItem newItem) newMonkey monkies
    ST.put (newMonkies, M.adjust (+1) n itemsInspected, supermod, wc)
    
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

parseMonkey :: (Monad m) => ParsecT Void String m (Int, Monkey)
parseMonkey = do
    string "Monkey "
    num <- parseInt
    char ':'
    eol
    string "  Starting items: "
    items <- sepBy1 parseInt (string ", ")
    eol
    string "  Operation: new = old "
    op <- parsePlus <|> parseTimes
    string "  Test: divisible by "
    test <- parseInt
    eol
    string "    If true: throw to monkey "
    trueCase <- parseInt
    eol
    string "    If false: throw to monkey "
    falseCase <- parseInt
    eol
    let cases = M.fromList [(True, trueCase), (False, falseCase)]
    return (num, Monkey items op test cases) 

parsePlus :: (Monad m) => ParsecT Void String m (Int -> Int)
parsePlus = do
             string "+ "
             val <- (some digitChar) <|> (string "old")
             eol
             case val of
                "old" -> return (\x -> x + x)
                _ -> return (+ (read val))

parseTimes :: (Monad m) => ParsecT Void String m (Int -> Int)
parseTimes = do
             string "* "
             val <- (some digitChar) <|> (string "old")
             eol
             case val of
                "old" -> return (\x -> x * x)
                _ -> return (* (read val))
