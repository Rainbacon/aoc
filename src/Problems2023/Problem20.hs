module Problems2023.Problem20 (runEasy, runHard) where

import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Data.Map as M
import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

data FlopState = On | Off
  deriving (Show)
data Pulse = High | Low
  deriving (Eq, Show)
data Module = Broadcast [String] | FlipFlop FlopState [String] | Conjunction (M.Map String Pulse) [String] 
  deriving (Show)
type Modules = M.Map String Module
type Counter = (Int, Int)
type ModuleState = (Counter, Modules, Bool)

flipFlop :: FlopState -> FlopState
flipFlop On = Off
flipFlop Off = On

targets :: Module -> [String]
targets (Broadcast xs) = xs
targets (FlipFlop _ xs) = xs
targets (Conjunction _ xs) = xs

runEasy :: FilePath -> IO String
runEasy fp = do
  input <- parseFile parseInput fp
  let ((cLow, cHigh), _, _) = ST.execState (runPulses' 1000) ((0, 0), input, False)
  return $ show $ cLow * cHigh
  

runHard :: FilePath -> IO String
runHard fp = do
  input <- parseFile parseInput fp
  return $ show $ ST.evalState (runPulses'' 1) ((0, 0), input, False)

runPulses'' :: Int -> ST.State ModuleState Int
runPulses'' n = do
  runPulses [("", "broadcaster", Low)]
  (_, _, seen) <- ST.get
  case seen of
    True -> return n
    False -> runPulses'' (n + 1)


runPulses' :: Int -> ST.State ModuleState ()
runPulses' 0 = return ()
runPulses' x = do
  runPulses [("", "broadcaster", Low)]
  runPulses' (x - 1)

runPulses :: [(String, String, Pulse)] -> ST.State ModuleState ()
runPulses [] = return ()
runPulses ((prev, curr@"rx", pulse@Low):queue) = do
  (counter, modules, seen) <- ST.get
  ST.put (counter, modules, True)
  let m = M.lookup curr modules
  case m of
    (Just modName) -> do
      pulses <- processPulse prev curr pulse modName
      runPulses (queue ++ pulses)
    Nothing -> do
      ST.put (increment counter pulse, modules, True)
      runPulses queue
runPulses ((prev, curr, pulse):queue) = do
  (counter, modules, seen) <- ST.get
  let m = M.lookup curr modules
  case m of
    (Just modName) -> do
      pulses <- processPulse prev curr pulse modName
      runPulses (queue ++ pulses)
    Nothing -> do
      ST.put (increment counter pulse, modules, seen)
      runPulses queue

-- TODO this needs to be stateful so that it can flip the flop state
processPulse :: String -> String -> Pulse -> Module -> ST.State ModuleState [(String, String, Pulse)] 
processPulse _ curr pulse (Broadcast xs) = do
  (counter, modules, seen) <- ST.get
  ST.put (increment counter pulse, modules, seen)
  return $ map (\x -> (curr, x, pulse)) xs
processPulse _ curr High (FlipFlop _ _) = do 
  (counter, modules, seen) <- ST.get
  ST.put (increment counter High, modules, seen)
  return []
processPulse _ curr Low (FlipFlop fs xs) = do
  (counter, modules, seen) <- ST.get
  let newModules = M.insert curr (FlipFlop (flipFlop fs) xs) modules
  ST.put (increment counter Low, newModules, seen)
  let pulse' = flopPulse fs
  return $ map (\x -> (curr, x, pulse')) xs
processPulse prev curr pulse (Conjunction inputs xs) = do
  (counter, modules, seen) <- ST.get
  let newInputs = M.insert prev pulse inputs
      allHigh = all (== High) newInputs
      pulse' = conjoinPulse allHigh
  ST.put (increment counter pulse, M.insert curr (Conjunction newInputs xs) modules, seen)
  return $ map (\x -> (curr, x, pulse')) xs

flopPulse :: FlopState -> Pulse
flopPulse On = Low
flopPulse Off = High

conjoinPulse :: Bool -> Pulse
conjoinPulse True = Low
conjoinPulse False = High

increment :: Counter -> Pulse -> Counter
increment (l, h) Low = (l + 1, h)
increment (l, h) High = (l, h + 1)

buildBacktrack :: Modules -> String -> Modules
buildBacktrack ms key = let m = Y.fromJust $ M.lookup key ms 
                            preds = M.filter (\val -> key `elem` (targets val)) ms
                            predKeys = M.map (\_ -> Low) preds
                        in case m of
                          (Conjunction callers tgts) -> M.insert key (Conjunction predKeys tgts) ms
                          _ -> ms

--- parsing ---
parseInput :: (Monad m) => ParsecT Void String m Modules
parseInput = do 
  modules <- sepEndBy1 parseModule eol
  let input = M.fromList modules
  return $ foldl buildBacktrack input (M.keys input)


parseModule :: (Monad m) => ParsecT Void String m (String, Module)
parseModule = parseBroadcast <|> parseOther

parseBroadcast :: (Monad m) => ParsecT Void String m (String, Module)
parseBroadcast = do
  name <- some letterChar
  string " -> "
  tgts <- sepBy1 (some letterChar) (string ", ")
  return $ (name, Broadcast tgts)

parseOther :: (Monad m) => ParsecT Void String m (String, Module)
parseOther = do
  constructor <- (char '%' *> pure (FlipFlop Off)) <|> (char '&' *> pure (Conjunction M.empty))
  name <- some letterChar
  string " -> "
  tgts <- sepBy1 (some letterChar) (string ", ")
  return $ (name, constructor tgts)