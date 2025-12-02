module Utils.Parsing (
    parseFile
  , parseFast
  , parseInt
  , parseInteger
  , parsePositive
) where


import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.IO.Class

parseFile :: (MonadIO m) => ParsecT Void String m a -> FilePath -> m a
parseFile parser filepath = do
    input <- liftIO $ readFile filepath
    result <- runParserT parser "Utils.hs" input
    case result of
        Left e -> error $ "Failed to parse: " ++ show e
        Right x -> return x

parseFast :: (MonadIO m) => (String -> a) -> FilePath -> m a
parseFast f fp = do
    input <- liftIO $ readFile fp
    return $ f input 

parseInt :: (Monad m) => ParsecT Void String m Int
parseInt = parsePositive <|> parseNegative

parseInteger :: (Monad m) => ParsecT Void String m Integer
parseInteger = do
    i <- parseInt
    return $ toInteger i

parsePositive :: (Monad m) => ParsecT Void String m Int
parsePositive = read <$> some digitChar

parseNegative :: (Monad m) => ParsecT Void String m Int
parseNegative = do
    char '-'
    ((-1) *) <$> parsePositive