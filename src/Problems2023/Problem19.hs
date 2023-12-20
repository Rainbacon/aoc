module Problems2023.Problem19 (runEasy, runHard) where

import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Data.Map as M
import Utils
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
type SankeyState = ([Range], Workflow)

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
    let initRange = M.fromList [('x', (0, 4000)), ('m', (0, 4000)), ('a', (0, 4000)), ('s', (0, 4000))]
    let initState = ([], flows)

runFlow' :: String -> Workflow -> Range -> ST.State SankeyState Range
runFlow' key range = do
    (_, flows) <- ST.get
    let rules = Y.fromJust $ M.lookup key flows
    foldM splitRange range rules

splitRange :: Range -> Rule -> ST.State SankeyState Range
splitRange range (Default Reject) = return $ M.empty
splitRange range (Default Accept) = do
    (accepted, flows) <- ST.get
    ST.put ((range:accepted), flows)
splitRange range (Default (Next res)) = runFlow' res range 

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
