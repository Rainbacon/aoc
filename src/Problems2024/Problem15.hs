module Problems2024.Problem15 (runEasy, runHard) where

import Utils.Parsing
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as M
import qualified Control.Monad.State as ST
import Data.Void
import Debug.Trace
import Data.List

data Unit = Wall | Empty | Box | Robot
    deriving (Eq)

instance Show Unit where
    show Wall = "#"
    show Empty = "."
    show Box = "O"
    show Robot = "@"

data Instruction = L | R | U | D
    deriving (Eq, Show)

type Warehouse = M.Map Point Unit

data WideUnit = Wa | Em | BL | BR | Rbt
    deriving (Eq)

instance Show WideUnit where
    show Wa = "#"
    show Em = "."
    show BL = "["
    show BR = "]"
    show Rbt = "@"

type WideWarehouse = M.Map Point WideUnit

runEasy :: FilePath -> IO String
runEasy fp = do
    (warehouse, instructions) <- parseFile parseInput fp
    let finalState = ST.execState (mapM_ runRobot instructions) warehouse
    return $ show $ sum $ map gps $ M.keys $ M.filter (== Box) finalState

gps :: Point -> Int
gps (x, y) = 100 * x + y

printWarehouse :: (Show a) => M.Map Point a -> String
printWarehouse wh = let pairs = M.assocs wh
                        grid = groupBy (\(a, _) (b, _) -> (fst a) == (fst b)) $ sortBy (\((a, b), _) ((c, d), _) -> compare a c <> compare b d) pairs
                    in unlines $ map (concatMap (show . snd)) grid 

runRobot :: Instruction -> ST.State Warehouse ()
runRobot ins = do
    warehouse <- ST.get
    let robot = head $ M.keys $ M.filter (== Robot) warehouse
        newWarehouse = pushSmall ins robot warehouse
    ST.put newWarehouse

pushSmall :: Instruction -> Point -> Warehouse -> Warehouse
pushSmall L (x, y) wh | next == Wall = wh
                      | next == Empty = foldl (\acc (k, v) -> M.insert k v acc) wh [((x, y - 1), curr), ((x, y), Empty)]
                      | otherwise = if canMove then pushSmall L (x, y) movedNext else wh
               where next = wh M.! (x, y - 1)
                     curr = wh M.! (x, y)
                     movedNext = pushSmall L (x, y - 1) wh 
                     canMove = movedNext M.! (x, y - 1) == Empty 
pushSmall R (x, y) wh | next == Wall = wh
                      | next == Empty = foldl (\acc (k, v) -> M.insert k v acc) wh [((x, y + 1), curr), ((x, y), Empty)]
                      | otherwise = if canMove then pushSmall R (x, y) movedNext else wh
               where next = wh M.! (x, y + 1)
                     curr = wh M.! (x, y)
                     movedNext = pushSmall R (x, y + 1) wh 
                     canMove = movedNext M.! (x, y + 1) == Empty 
pushSmall U (x, y) wh | next == Wall = wh
                      | next == Empty = foldl (\acc (k, v) -> M.insert k v acc) wh [((x - 1, y), curr), ((x, y), Empty)]
                      | otherwise = if canMove then pushSmall U (x, y) movedNext else wh
               where next = wh M.! (x - 1, y)
                     curr = wh M.! (x, y)
                     movedNext = pushSmall U (x - 1, y) wh 
                     canMove = movedNext M.! (x - 1, y) == Empty 
pushSmall D (x, y) wh | next == Wall = wh
                      | next == Empty = foldl (\acc (k, v) -> M.insert k v acc) wh [((x + 1, y), curr), ((x, y), Empty)]
                      | otherwise = if canMove then pushSmall D (x, y) movedNext else wh
               where next = wh M.! (x + 1, y)
                     curr = wh M.! (x, y)
                     movedNext = pushSmall D (x + 1, y) wh 
                     canMove = movedNext M.! (x + 1, y) == Empty 

runHard :: FilePath -> IO String
runHard fp = do 
    (warehouse, instructions) <- parseFile parseWide fp
    let finalState = ST.execState (mapM_ runRobot' instructions) warehouse
    return $ show $ sum $ map gps $ M.keys $ M.filter (== BL) finalState

runRobot' :: Instruction -> ST.State WideWarehouse ()
runRobot' ins = do
    warehouse <- ST.get
    let robot = head $ M.keys $ M.filter (== Rbt) warehouse
        newWarehouse = pushLarge ins robot warehouse
    ST.put newWarehouse

pushLarge :: Instruction -> Point -> WideWarehouse -> WideWarehouse
pushLarge L (x, y) wh | next == Wa = wh
                      | next == Em = foldl (\acc (k, v) -> M.insert k v acc) wh [((x, y - 1), curr), ((x, y), Em)]
                      | otherwise = if canMove then pushLarge L (x, y) movedNext else wh
               where next = wh M.! (x, y - 1)
                     curr = wh M.! (x, y)
                     movedNext = pushLarge L (x, y - 1) wh 
                     canMove = movedNext M.! (x, y - 1) == Em
pushLarge R (x, y) wh | next == Wa = wh
                      | next == Em = foldl (\acc (k, v) -> M.insert k v acc) wh [((x, y + 1), curr), ((x, y), Em)]
                      | otherwise = if canMove then pushLarge R (x, y) movedNext else wh
               where next = wh M.! (x, y + 1)
                     curr = wh M.! (x, y)
                     movedNext = pushLarge R (x, y + 1) wh 
                     canMove = movedNext M.! (x, y + 1) == Em
pushLarge ins (x, y) wh | canMove = moveWide ins wh (x, y)
                        | otherwise = wh
                      where canMove = canMoveLarge ins wh (f x, y) 
                            f = case ins of
                                    U -> (\a -> a - 1)
                                    D -> (+ 1)
                                    otherwise -> id

moveWide :: Instruction -> WideWarehouse -> Point -> WideWarehouse
moveWide ins wh (x, y) | next == Em = foldl (\acc (k, v) -> M.insert k v acc) wh [((f x, y), curr), ((x, y), Em)]
                       | next == BL = moveWide ins movedLeft (x, y)
                       | next == BR = moveWide ins movedRight (x, y)
                     where curr = wh M.! (x, y)
                           next = wh M.! (f x, y)
                           movedLeft = foldl (\acc p -> moveWide ins acc p) wh [(f x, y), (f x, y + 1)]
                           movedRight = foldl (\acc p -> moveWide ins acc p) wh [(f x, y), (f x, y - 1)]
                           f = case ins of
                                U -> (\a -> a - 1)
                                D -> (+ 1)
                                otherwise -> id


canMoveLarge :: Instruction -> WideWarehouse -> Point -> Bool
canMoveLarge ins wh (x, y) | curr == Wa = False
                           | curr == Em = True
                           | curr == BL = all id $ map (canMoveLarge ins wh) [(f x, y), (f x, y + 1)] 
                           | curr == BR = all id $ map (canMoveLarge ins wh) [(f x, y), (f x, y - 1)]
                           | otherwise = error "There are two robots????"
                         where curr = wh M.! (x, y)
                               f = case ins of
                                    U -> (\a -> a - 1)
                                    D -> (+ 1)
                                    otherwise -> id


parseInput :: (Monad m) => ParsecT Void String m (Warehouse, [Instruction])
parseInput = do
    grid <- sepEndBy1 parseGridRow eol
    instructions <- sepEndBy1 (many parseInstruction) eol
    return (M.fromList $ mapPos grid, concat instructions)

parseGridRow :: (Monad m) => ParsecT Void String m [Unit]
parseGridRow = many parseUnit

parseUnit :: (Monad m) => ParsecT Void String m Unit
parseUnit = (char '#' >> return Wall) <|> (char '.' >> return Empty) <|> (char 'O' >> return Box) <|> (char '@' >> return Robot)

parseInstruction :: (Monad m) => ParsecT Void String m Instruction
parseInstruction = (char '<' >> return L) <|> (char '>' >> return R) <|> (char '^' >> return U) <|> (char 'v' >> return D)

parseWide :: (Monad m) => ParsecT Void String m (WideWarehouse, [Instruction])
parseWide = do
    gridRaw <- sepEndBy1 parseGridRow eol
    let grid = map (concatMap expand) gridRaw
    instructions <- sepEndBy1 (many parseInstruction) eol
    return (M.fromList $ mapPos grid, concat instructions)

expand :: Unit -> [WideUnit]
expand Wall = [Wa, Wa]
expand Empty = [Em, Em]
expand Box = [BL, BR]
expand Robot = [Rbt, Em]