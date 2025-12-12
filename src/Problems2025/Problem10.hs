module Problems2025.Problem10 (runEasy, runHard) where

import Utils.Parsing
import Utils.Search
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M

data Light = Off | On
    deriving (Eq, Ord, Show)

toggle :: Light -> Light
toggle On = Off
toggle Off = On

off :: Light -> Light
off _ = Off

on :: Light -> Light
on _ = On

data Machine = Machine {
    lights :: M.Map Int Light
  , buttons :: [[Int]]
  , joltages :: [Int]
}

runEasy :: FilePath -> IO String
runEasy fp = do
    machines <- parseFile (sepEndBy1 parseMachine eol) fp
    return $ show $ take 65 $ map reachTarget machines

reachTarget :: Machine -> Int
reachTarget (Machine ls bs _) = bfs (M.map off ls) ls (pushButtons bs)

pushButtons :: [[Int]] -> M.Map Int Light -> [M.Map Int Light]
pushButtons btns initial = map (pushButton initial) btns

pushButton :: M.Map Int Light -> [Int] -> M.Map Int Light
pushButton ls b = foldl (\l i -> M.adjust toggle i l) ls b

runHard :: FilePath -> IO String
runHard _ = return ""

parseMachine :: (Monad m) => ParsecT Void String m Machine
parseMachine = do
    ls <- char '[' >> some parseLight <* string "] "
    bs <- sepEndBy1 parseButton (char ' ')
    js <- char '{' >> (sepBy1 parseInt (char ',')) <* char '}'
    return $ Machine (M.fromList $ zip [0..] ls) bs js

parseLight :: (Monad m) => ParsecT Void String m Light
parseLight = (char '.' >> pure Off) <|> (char '#' >> pure On)

parseButton :: (Monad m) => ParsecT Void String m [Int]
parseButton = do
    char '('
    indices <- sepBy1 parseInt (char ',')
    char ')'
    return indices