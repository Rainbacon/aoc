module Problems2022.Problem16 where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type ValveName = String
data Valve = Valve {
    name :: ValveName
  , flow :: Int
  , open :: Bool
  , pipes :: [ValveName]
} deriving (Show)

type Valves = M.Map ValveName Valve
type ValveDistances = M.Map (ValveName, ValveName) Int

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ runValves 30 (M.fromList input) "AA"

runHard :: FilePath -> IO String
runHard _ = return ""

openValve :: ValveName -> Valves -> Valves
openValve vn vs = let (Valve f _ ps) = Y.fromJust $ M.lookup vn vs
                  in M.insert vn (Valve f True ps) vs

buildDistance :: Valves -> M.Map (ValveName, ValveName) Int
buildDistance valves = let vs = M.keys valves
                           pairs = filter notSame $ (map (,) vs) <*>vs 
                       in foldl (distance valves) M.empty pairs 

distance :: Valves -> ValveDistances -> (Valve, Valve) -> ValveDistances
distance valves distances ((Valve x _ _ _), (Valve y _ _ _)) = case M.lookup (y, x) distances of
                                                                Just i ->  error "oops" -- M.append (x, y) i distances
                                                                Nothing -> error "oops"

notSame :: Valve -> Valve -> Bool
notSame (Valve x _ _ _) (Valve y _ _ _) = x /= y

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
