module Problems2015.Problem18 (runEasy, runHard) where

import Utils.Parsing
import Utils.Grid
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import qualified Data.Maybe as Y

data Light = On | Off

isOn :: Light -> Bool
isOn On = True
isOn _ = False

instance GridShow Light where
    toChar On = '#'
    toChar Off = '.'

runEasy :: FilePath -> IO String
runEasy fp = do
    lights <- parseFile parseLights fp
    putStrLn "How many steps?"
    numSteps <- read <$> getLine
    return $ show $ M.size $ M.filter isOn $ (iterate step lights) !! numSteps

step :: (Grid Light) -> (Grid Light)
step grid = M.mapWithKey (toggle grid) grid

toggle :: (Grid Light) -> Point -> Light -> Light
toggle grid p On | (numNeighbors grid p) == 2 = On
                 | (numNeighbors grid p) == 3 = On
                 | otherwise = Off
toggle grid p Off | (numNeighbors grid p) == 3 = On
                  | otherwise = Off

numNeighbors :: (Grid Light) -> Point -> Int
numNeighbors grid point = length $ filter (isOn . Y.fromJust) realNeighbors 
    where realNeighbors = filter Y.isJust $ map (\p -> M.lookup p grid) (neighbors point)

runHard :: FilePath -> IO String
runHard fp = do
    lights <- parseFile parseLights fp
    let lights' = foldl (\g p -> M.insert p On g) lights [(0,0),(99,0),(0,99),(99,99)]
    putStrLn "How many steps?"
    numSteps <- read <$> getLine
    return $ show $ M.size $ M.filter isOn $ (iterate step' lights') !! numSteps

step' :: (Grid Light) -> (Grid Light)
step' grid = M.mapWithKey (toggle' grid) grid

toggle' :: (Grid Light) -> Point -> Light -> Light
toggle' _ (0, 0) _ = On
toggle' _ (0, 99) _ = On
toggle' _ (99, 0) _ = On
toggle' _ (99, 99) _ = On
toggle' g p l = toggle g p l

parseLights :: (Monad m) => ParsecT Void String m (Grid Light)
parseLights = sepEndBy1 (some parseLight) eol >>= (return . constructGrid)

parseLight :: (Monad m) => ParsecT Void String m Light
parseLight = (char '#' >> return On) <|> (char '.' >> return Off)