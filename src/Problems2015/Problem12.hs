module Problems2015.Problem12 (runEasy, runHard) where

import Utils (parseFile, parseInt)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M

data Json = Object (M.Map String Json) | Arr [Json] | S String | I Int
    deriving (Show)

fromS :: Json -> String
fromS (S s) = s
fromS j = error "called fromS with " ++ show j

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ sum input

runHard :: FilePath -> IO String
runHard fp = do
    json <- parseFile parseJson fp
    return $ show $ eval json

eval :: Json -> Int
eval (I i) = i
eval (S _) = 0
eval (Arr js) = sum $ map eval js
eval (Object kvs) = let hasRed = any isRed kvs
    in case hasRed of
        True -> 0
        False -> sum $ map eval (M.elems kvs)

isRed :: Json -> Bool
isRed (S "red") = True
isRed _ = False


parseInput :: (Monad m) => ParsecT Void String m [Int]
parseInput = do
    ints <- many (parseGarbage <|> parseInt)
    eof
    return ints

parseGarbage :: (Monad m) => ParsecT Void String m Int
parseGarbage = some (letterChar <|> (oneOf "[]{},:\"")) >> return 0

parseJson :: (Monad m) => ParsecT Void String m Json
parseJson = parseObject <|> parseArr <|> parseS <|> parseI

parseI :: (Monad m) => ParsecT Void String m Json
parseI = parseInt >>= (\i -> return (I i))

parseS :: (Monad m) => ParsecT Void String m Json
parseS = do
    char '"'
    s <- some letterChar
    char '"'
    return $ S s

parseArr :: (Monad m) => ParsecT Void String m Json
parseArr = do
    char '['
    js <- sepBy parseJson (char ',')
    char ']'
    return $ Arr js

parseObject :: (Monad m) => ParsecT Void String m Json
parseObject = do
    char '{'
    mapping <- sepBy parseKeyVal (char ',')
    char '}'
    return $ Object (M.fromList mapping)

parseKeyVal :: (Monad m) => ParsecT Void String m (String, Json)
parseKeyVal = do
    key <- parseS
    char ':'
    val <- parseJson
    return (fromS key, val)