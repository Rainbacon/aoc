module Problems2015.Problem18 (runEasy, runHard) where

import Utils.Parsing
import Utils.Grid
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M

data Light = On | Off

instance GridShow Light where
    toChar On = '#'
    toChar Off = '.'

runEasy :: FilePath -> IO String
runEasy fp = do
    lights <- parseFile parseLights fp
    return $ displayGrid lights

runHard :: FilePath -> IO String
runHard _ = return ""

parseLights :: (Monad m) => ParsecT Void String m (Grid Light)
parseLights = sepEndBy1 (some parseLight) eol >>= (return . constructGrid)

parseLight :: (Monad m) => ParsecT Void String m Light
parseLight = (char '#' >> return On) <|> (char '.' >> return Off)