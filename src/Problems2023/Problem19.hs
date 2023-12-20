module Problems2023.Problem19 (runEasy, runHard) where

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
data Rule = Rule (Part -> Bool) RuleResult

isNext :: RuleResult -> Bool
isNext (Next s) = True
isNext _ = False

fromNext :: RuleResult -> String
fromNext (Next s) = s
fromNext res = error $ "fromNext called with " ++ show res

data Part = Part {
    getX :: Int
  , getM :: Int
  , getA :: Int
  , getS :: Int
}

runEasy :: FilePath -> IO String
runEasy fp = do
    (flows, parts) <- parseFile parseInput fp
    let accepted = filter (checkPart flows) parts
    return $ show $ sum $ map scorePart accepted

runHard :: FilePath -> IO String
runHard _ = return ""

scorePart :: Part -> Int
scorePart (Part a b c d) = a + b + c + d

checkPart :: Workflow -> Part -> Bool
checkPart flows part = let result = runFlow "in" flows part
                       in case result of
                            Accept -> True
                            Reject -> False

runFlow :: String -> Workflow -> Part -> RuleResult
runFlow key flows part | isNext res = runFlow (fromNext res) flows part
                       | otherwise = res
                     where rules = Y.fromJust $ M.lookup key flows
                           res = snd $ head $ filter (\(a, _) -> a) $ map (\(Rule fn r) -> (fn part, r)) rules


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
    part <- (char 'a' *> pure getA) <|> (char 'm' *> pure getM) <|> (char 'x' *> pure getX) <|> (char 's' *> pure getS)
    fn <- (char '<' *> pure (<)) <|> (char '>' *> pure (>))
    num <- parseInt
    char ':'
    result <- (char 'A' *> pure Accept) <|> (char 'R' *> pure Reject) <|> parseNext
    return $ Rule (\p -> fn (part p) num) result

parseNext :: (Monad m) => ParsecT Void String m RuleResult
parseNext = do
    tgt <- some letterChar
    return $ Next tgt

parseDefault :: (Monad m) => ParsecT Void String m Rule
parseDefault = do
    target <- (char 'A' *> pure Accept) <|> (char 'R' *> pure Reject) <|> parseNext
    return $ Rule (\_ -> True) target

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
    return $ Part x m a s
