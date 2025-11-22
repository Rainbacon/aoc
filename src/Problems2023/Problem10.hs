module Problems2023.Problem10 where

import Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Utils

data Pipe = V | H | NE | NW | SW | SE | G | Start
type PipeMap = M.Map Point Pipe
type LoopState = (PipeMap, S.Set Point, Point)
type Loop = [Point]
type Loop2 = S.Set Point
type Inside = Bool

runEasy :: FilePath -> IO String
runEasy fp = do
    pipes <- parseFile parseInput fp
    let start@(x, y) = head $ M.keys $ M.filter isStart pipes
    let initialState = (pipes, S.singleton start, start)
    let loops = map (findLoop initialState) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    let loop = Y.fromJust $ head $ filter Y.isJust loops
    return $ show $ length loop `div` 2

runHard :: FilePath -> IO String
runHard fp = do
    pipes <- parseFile parseInput fp
    let start@(x, y) = head $ M.keys $ M.filter isStart pipes
    let initialState = (pipes, S.singleton start, start)
    let loops = map (findLoop initialState) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    let loop = S.fromList $ Y.fromJust $ head $ filter Y.isJust loops
    return $ show $ countInside loop pipes

countInside :: Loop2 -> PipeMap -> Int
countInside loop pipes = length $ filter id checked
                     where allPoints = M.keys pipes 
                           cols = maximum $ map snd allPoints
                           checked = map (isInside loop pipes cols) allPoints

isInside :: Loop2 -> PipeMap -> Int -> Point -> Bool
isInside loop pipes cols p@(x, y) | S.member p loop = False
                                  | otherwise = odd $ length walls
                                where restOfLine = zip (repeat x) [y+1..cols]
                                      walls = process loop pipes restOfLine

process :: Loop2 -> PipeMap -> [Point] -> [Point]
process loop pipes [] = []
process loop pipes (p:ps) | onLoop && isV pipe = p : rest
                          | onLoop && isTurn pipe && jump = p : rest'
                          | otherwise = rest
                        where pipe = Y.fromJust $ M.lookup p pipes
                              rest = process loop pipes ps
                              onLoop = S.member p loop
                              cutH = dropWhile (\p2 -> isH $ Y.fromJust $ M.lookup p2 pipes) ps
                              nextTurn = head cutH
                              pipe2 = Y.fromJust $ M.lookup nextTurn pipes
                              jump = isJump pipe pipe2
                              rest' = process loop pipes (tail cutH)

isJump :: Pipe -> Pipe -> Bool
isJump NE SW = True
isJump SE NW = True
isJump _ _ = False

isTurn :: Pipe -> Bool
isTurn NE = True
isTurn NW = True
isTurn SE = True
isTurn SW = True
isTurn _ = False

isH :: Pipe -> Bool
isH H = True
isH _ = False

isV :: Pipe -> Bool
isV V = True
isV Start = True
isV _ = False

isStart :: Pipe -> Bool
isStart Start = True
isStart _ = False

findLoop :: LoopState -> Point -> Maybe Loop
findLoop initialState p = ST.evalState (findLoop' (Just p)) initialState

findLoop' :: Maybe Point -> ST.State LoopState (Maybe Loop)
findLoop' Nothing = return Nothing
findLoop' (Just curr@(x, y)) = do
    (pipes, seen, prev@(px, py)) <- ST.get
    let isLoop = S.member curr seen
    let currentPipe = M.lookup curr pipes
    let nextPipe = getNext prev curr currentPipe
    ST.put (pipes, S.insert curr seen, curr)
    if isLoop 
    then return $ Just (S.elems seen)
    else findLoop' nextPipe

getNext :: Point -> Point -> Maybe Pipe -> Maybe Point
getNext _ _ Nothing = Nothing
getNext (x1, y1) (x2, y2) (Just V) | x2 > x1 = Just (x2 + 1, y2)
                                   | x2 < x1 = Just (x2 - 1, y2)
                                   | otherwise = Nothing
getNext (x1, y1) (x2, y2) (Just H) | y2 > y1 = Just (x2, y2 + 1)
                                   | y2 < y1 = Just (x2, y2 - 1)
                                   | otherwise = Nothing
getNext (x1, y1) (x2, y2) (Just NE) | x2 > x1 && y2 == y1 = Just (x2, y2 + 1)
                                    | x2 == x1 && y2 < y1 = Just (x2 - 1, y2)
                                    | otherwise = Nothing
getNext (x1, y1) (x2, y2) (Just NW) | x2 > x1 && y2 == y1 = Just (x2, y2 - 1)
                                    | x2 == x1 && y2 > y1 = Just (x2 - 1, y2)
                                    | otherwise = Nothing
getNext (x1, y1) (x2, y2) (Just SW) | x2 < x1 && y2 == y1 = Just (x2, y2 - 1)
                                    | x2 == x1 && y2 > y1 = Just (x2 + 1, y2)
                                    | otherwise = Nothing
getNext (x1, y1) (x2, y2) (Just SE) | x2 < x1 && y2 == y1 = Just (x2, y2 + 1)
                                    | x2 == x1 && y2 < y1 = Just (x2 + 1, y2)
                                    | otherwise = Nothing
getNext _ _ (Just G) = Nothing
getNext _ _ (Just Start) = Nothing
                                                       

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m PipeMap
parseInput = do
    pipes <- sepEndBy1 (many parsePipe) eol 
    return $ M.fromList $ mapPos pipes


parsePipe :: (Monad m) => ParsecT Void String m Pipe
parsePipe = parseV <|> parseH <|> parseNE <|> parseNW <|> parseSW <|> parseSE <|> parseG <|> parseS

parseV :: (Monad m) => ParsecT Void String m Pipe
parseV = char '|' *> pure V

parseH :: (Monad m) => ParsecT Void String m Pipe
parseH = char '-' *> pure H

parseNE :: (Monad m) => ParsecT Void String m Pipe
parseNE = char 'L' *> pure NE

parseNW :: (Monad m) => ParsecT Void String m Pipe
parseNW = char 'J' *> pure NW

parseSW :: (Monad m) => ParsecT Void String m Pipe
parseSW = char '7' *> pure SW

parseSE :: (Monad m) => ParsecT Void String m Pipe
parseSE = char 'F' *> pure SE

parseG :: (Monad m) => ParsecT Void String m Pipe
parseG = char '.' *> pure G

parseS :: (Monad m) => ParsecT Void String m Pipe
parseS = char 'S' *> pure Start