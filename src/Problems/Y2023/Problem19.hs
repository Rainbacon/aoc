{-# LANGUAGE ViewPatterns #-}
module Problems.Y2023.Problem19 (runEasy, runHard) where

import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Data.Map as M
import Utils.Parsing
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

type Workflow = M.Map String [Rule]

data RuleResult = Accept | Reject | Next String
    deriving (Show)
data Rule = Greater Char Int RuleResult | Less Char Int RuleResult | Default RuleResult
    deriving (Show)
type Range = M.Map Char (Int, Int)
type Mapping = (Range, RuleResult)
type SankeyState = ([Range], Workflow, [(Range, RuleResult)])

isNext :: RuleResult -> Bool
isNext (Next s) = True
isNext _ = False

fromNext :: RuleResult -> String
fromNext (Next s) = s
fromNext res = error $ "fromNext called with " ++ show res

type Part = M.Map Char Int

runEasy :: FilePath -> IO String
runEasy fp = do
    (flows, parts) <- parseFile parseInput fp
    let accepted = filter (checkPart flows) parts
    return $ show $ sum $ map scorePart accepted

runHard :: FilePath -> IO String
runHard fp = do
    (flows, _) <- parseFile parseInput fp
    let initRange = M.fromList [('x', (1, 4000)), ('m', (1, 4000)), ('a', (1, 4000)), ('s', (1, 4000))]
    let initState = ([], flows, [(initRange, Next "in")])
    let (accepted, _, _) = ST.execState runFlows initState
    return $ show $ sum $ map countRange accepted

countRange :: Range -> Int
countRange range = product $ map (\(l, h) -> h - l + 1) $ M.elems range

runFlows :: ST.State SankeyState Range
runFlows = do
    (accepted, flows, queue) <- ST.get
    case queue of
        [] -> return $ M.empty
        ((range, Accept):qs) -> do
            ST.put (range:accepted, flows, qs)
            runFlows
        ((range, Reject):qs) -> do
            ST.put (accepted, flows, qs)
            runFlows
        ((range, res):qs) -> do
            ST.put (accepted, flows, qs)
            let key = fromNext res
            let rules = Y.fromJust $ M.lookup key flows
            ST.foldM splitRange range rules
            runFlows

splitRange :: Range -> Rule -> ST.State SankeyState Range
splitRange (M.null -> True) _ = return $ M.empty
splitRange range (Default Reject) = return $ M.empty
splitRange range (Default Accept) = do
    (accepted, flows, queue) <- ST.get
    ST.put ((range:accepted), flows, queue)
    return $ M.empty
splitRange range (Default res) = do
    (accepted, flows, queue) <- ST.get
    ST.put (accepted, flows, (range, res):queue)
    return $ M.empty
splitRange range (Greater c n res) = do
    (accepted, flows, queue) <- ST.get
    let (low, high) = Y.fromJust $ M.lookup c range
    let expanded = [low..high]
    let putR = filter (> n) expanded
    let retR = filter (<= n) expanded
    case length putR of
        0 -> return range
        _ -> do 
            let newLow = head putR
            let newHigh = head $ reverse putR
            let newRange = M.insert c (newLow, newHigh) range
            ST.put (accepted, flows, (newRange, res):queue)
            let newLow' = head retR
            let newHigh' = head $ reverse retR
            let newRange' = M.insert c (newLow', newHigh') range
            return newRange'
splitRange range (Less c n res) = do
    (accepted, flows, queue) <- ST.get
    let (low, high) = Y.fromJust $ M.lookup c range
    let expanded = [low..high]
    let putR = filter (< n) expanded
    let retR = filter (>= n) expanded
    case length putR of
        0 -> return range
        _ -> do 
            let newLow = head putR
            let newHigh = head $ reverse putR
            let newRange = M.insert c (newLow, newHigh) range
            ST.put (accepted, flows, (newRange, res):queue)
            let newLow' = head retR
            let newHigh' = head $ reverse retR
            let newRange' = M.insert c (newLow', newHigh') range
            return newRange'


scorePart :: Part -> Int
scorePart part = sum $ M.elems part

checkPart :: Workflow -> Part -> Bool
checkPart flows part = let result = runFlow "in" flows part
                       in case result of
                            Accept -> True
                            Reject -> False

runFlow :: String -> Workflow -> Part -> RuleResult
runFlow key flows part | isNext res = runFlow (fromNext res) flows part
                       | otherwise = res
                     where rules = Y.fromJust $ M.lookup key flows
                           res = snd $ head $ filter (\(a, _) -> a) $ map (applyRule part) rules

applyRule :: Part -> Rule -> (Bool, RuleResult)
applyRule part (Greater c n r) | p > n = (True, r)
                               | otherwise = (False, r)
                             where p = Y.fromJust $ M.lookup c part
applyRule part (Less c n r) | p < n = (True, r)
                            | otherwise = (False, r)
                          where p = Y.fromJust $ M.lookup c part
applyRule part (Default r) = (True, r)


--- parsing ---
parseInput :: (Monad m) => ParsecT Void String m (Workflow, [Part])
parseInput = do
    workflows <- sepEndBy1 parseWorkflow eol
    eol
    parts <- sepEndBy1 parsePart eol
    return (M.fromList workflows, parts)

parseWorkflow :: (Monad m) => ParsecT Void String m (String, [Rule])
parseWorkflow = do
    flowName <- trace ("parsing workflow") $ some letterChar
    char '{'
    rules <- sepBy1 ((try parseRule) <|> parseDefault) (char ',')
    char '}' 
    return (flowName, rules)

parseRule :: (Monad m) => ParsecT Void String m Rule
parseRule = do
    part <- (char 'a') <|> (char 'm') <|> (char 'x') <|> (char 's')
    constructor <- (char '<' *> pure (Less)) <|> (char '>' *> pure (Greater))
    num <- parseInt
    char ':'
    result <- (char 'A' *> pure Accept) <|> (char 'R' *> pure Reject) <|> parseNext
    return $ constructor part num result

parseNext :: (Monad m) => ParsecT Void String m RuleResult
parseNext = do
    tgt <- some letterChar
    return $ Next tgt

parseDefault :: (Monad m) => ParsecT Void String m Rule
parseDefault = do
    target <- (char 'A' *> pure Accept) <|> (char 'R' *> pure Reject) <|> parseNext
    return $ Default target

parsePart :: (Monad m) => ParsecT Void String m Part
parsePart = do
    string "{x="
    x <- parseInt
    string ",m="
    m <- parseInt
    string ",a="
    a <- parseInt
    string ",s="
    s <- parseInt
    char '}'
    return $ M.fromList [('x', x), ('m', m), ('a', a), ('s', s)]
