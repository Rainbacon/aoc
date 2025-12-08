module Problems2025.Problem6 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Maybe as Y
import Control.Applicative (liftA2)
import Text.Read
import Debug.Trace

type BinOp = Int -> Int -> Int

runEasy :: FilePath -> IO String
runEasy fp = do
    (nums, ops) <- parseFile parseProblems fp
    return $ show $ sum $ foldl1 (zipWith3 applyBin ops) nums

applyBin :: BinOp -> Int -> Int -> Int
applyBin op x y = op x y

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFast lines fp
    let opStr = head $ reverse input
    let numStrs = reverse $ tail $ reverse input
    ops <- runParserT (parseRow parseOp) "2025-day-6" opStr
    case ops of
        Left e -> error $ "Failed to parse: " ++ show e
        Right x -> return $ show $ foldl1 (liftA2 (+)) $ applyFns x $ toInts numStrs
    

toInts :: [String] -> [Maybe Int]
toInts [] = []
toInts ([]:xs) = []
toInts xs = mInt:toInts rest
    where mInt = readMaybe (map head xs)
          rest = map tail xs

applyFns :: [BinOp] -> [Maybe Int] -> [Maybe Int]
applyFns [] _ = []
applyFns _ [] = []
applyFns (op:ops) xs = val : applyFns ops rest
    where val = foldl1 (liftA2 op) vals
          vals = takeWhile Y.isJust xs
          rest = tail $ dropWhile Y.isJust xs

parseProblems :: (Monad m) => ParsecT Void String m ([[Int]], [BinOp])
parseProblems = do
    nums <- sepEndBy1 (parseRow parseInt) eol
    ops <- parseRow parseOp
    return (nums, ops)

parseRow :: (Monad m) => ParsecT Void String m a -> ParsecT Void String m [a]
parseRow p = do 
    many (char ' ')
    items <- sepEndBy1 p (some (char ' ')) 
    return items

parseOp :: (Monad m) => ParsecT Void String m BinOp
parseOp = (char '*' >> pure (*)) <|> (char '+' >> pure (+))