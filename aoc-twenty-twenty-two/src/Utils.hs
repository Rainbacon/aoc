module Utils where

import Control.Monad.IO.Class
import Data.Void
import Text.Megaparsec

parseFile :: (MonadIO m) => ParsecT Void String m a -> FilePath -> m a
parseFile parser filepath = do
    input <- liftIO $ readFile filepath
    result <- runParserT parser "Utils.hs" input
    case result of
        Left e -> error $ "Failed to parse: " ++ show e
        Right x -> return x
