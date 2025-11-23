module Problems2015.Problem18 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M

data Light = On | Off

instance Show Light where
    show On = "#"
    show Off = "."

type LightArray = M.Map Point Light

runEasy :: FilePath -> IO String
runEasy fp = do
    lights <- parseFile parseLights fp
    return $ show lights

runHard :: FilePath -> IO String
runHard _ = return ""

parseLights :: (Monad m) => ParsecT Void String m LightArray
parseLights = sepEndBy1 (some parseLight) eol >>= (return . M.fromList . mapPos)

parseLight :: (Monad m) => ParsecT Void String m Light
parseLight = (char '#' >> return On) <|> (char '.' >> return Off)