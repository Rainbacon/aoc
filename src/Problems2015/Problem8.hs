module Problems2015.Problem8 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Code = (Int, Int)

runEasy :: FilePath -> IO String
runEasy fp = do
    codes <- parseFile parseEasy fp
    return $ show $ (\(a, b) -> a - b) $ foldl sum' (0, 0) codes

runHard :: FilePath -> IO String
runHard fp = do
    codes <- parseFile parseHard fp
    return $ show $ (\(a, b) -> a - b) $ foldl sum' (0, 0) codes

parseEasy :: (Monad m) => ParsecT Void String m [Code]
parseEasy = sepEndBy1 parseCode eol

parseCode :: (Monad m) => ParsecT Void String m Code
parseCode = do
    parts <- many ((try parseEscaped) <|> parseQuote' <|> parseLetter)
    return $ foldl sum' (0, 0) parts

parseQuote' :: (Monad m) => ParsecT Void String m Code
parseQuote' = do
    char '"' 
    return $ (1, 0)

parseLetter :: (Monad m) => ParsecT Void String m Code
parseLetter = do
    letterChar 
    return $ (1, 1)

sum' :: Code -> Code -> Code
sum' (a, b) (c, d) = (a + c, b + d)

parseEscaped :: (Monad m) => ParsecT Void String m Code
parseEscaped = do
    char '\\'
    codeLen <- parseSlash <|> parseQuote <|> parseAscii
    return $ codeLen

parseSlash :: (Monad m) => ParsecT Void String m Code
parseSlash = do 
    char '\\'
    return $ (2, 1)

parseQuote :: (Monad m) => ParsecT Void String m Code
parseQuote = do
    char '"'
    return $ (2, 1)

parseAscii :: (Monad m) => ParsecT Void String m Code
parseAscii = do
    count 3 alphaNumChar
    return $ (4, 1)



parseHard :: (Monad m) => ParsecT Void String m [Code]
parseHard = sepEndBy1 parseCode' eol

parseCode' :: (Monad m) => ParsecT Void String m Code
parseCode' = do
    parts <- many ((try parseEscaped') <|> parseQuote'' <|> parseLetter)
    return $ foldl sum' (0, 0) parts

parseQuote'' :: (Monad m) => ParsecT Void String m Code
parseQuote'' = do
    char '"' 
    return $ (3, 1)

parseEscaped' :: (Monad m) => ParsecT Void String m Code
parseEscaped' = do
    char '\\'
    codeLen <- parseSlash' <|> parseQuote''' <|> parseAscii'
    return $ codeLen

parseSlash' :: (Monad m) => ParsecT Void String m Code
parseSlash' = do 
    char '\\'
    return $ (4, 2)

parseQuote''' :: (Monad m) => ParsecT Void String m Code
parseQuote''' = do
    char '"'
    return $ (4, 2)

parseAscii' :: (Monad m) => ParsecT Void String m Code
parseAscii' = do
    count 3 alphaNumChar
    return $ (5, 4)