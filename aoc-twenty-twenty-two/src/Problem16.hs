module Problem16 where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type ValveName = String
data Valve = Valve {
    flow :: Int
  , open :: Bool
  , pipes :: [ValveName]
} deriving (Show)

type Valves = M.Map ValveName Valve

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ runValves 30 (M.fromList input) "AA"

runHard :: FilePath -> IO String
runHard _ = return ""

runValves :: Int -> Valves -> ValveName -> Int
runValves 0 _ _ = 0
runValves n vs vn | allOpen = 0
                  | open currentValve = maximum $ map (runValves (n - 1) vs) $ pipes currentValve
                  | otherwise = maximum $ (ifOpened:ifClosed)
                where currentValve = Y.fromJust $ M.lookup vn vs 
                      allOpen = foldr (\x acc -> open x && acc) True vs
                      ifOpened = (n - 1) * (flow currentValve) + (runValves (n - 1) (openValve vn vs) vn)
                      ifClosed = map (runValves (n - 1) vs) $ pipes currentValve

openValve :: ValveName -> Valves -> Valves
openValve vn vs = let (Valve f _ ps) = Y.fromJust $ M.lookup vn vs
                  in M.insert vn (Valve f True ps) vs

------------------
-- Parsing Code --
------------------
parseInput :: (Monad m) => ParsecT Void String m [(ValveName, Valve)]
parseInput = sepEndBy1 parseValve eol
    where parseValve = do
                string "Valve "
                valveName <- parseValveName
                string " has flow rate="
                flowRate <- parseInt
                parseTunnel <|> parseTunnels
                pipes <- sepBy parseValveName (string ", ")
                return $ (valveName, Valve flowRate False pipes)

parseValveName :: (Monad m) => ParsecT Void String m ValveName
parseValveName = some letterChar

parseTunnel :: (Monad m) => ParsecT Void String m String
parseTunnel = string "; tunnel leads to valve "

parseTunnels :: (Monad m) => ParsecT Void String m String
parseTunnels = string "; tunnels lead to valves "
