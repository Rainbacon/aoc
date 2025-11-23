module Problems2022.Problem10 where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

data Instruction = Noop | AddX Int
    deriving (Show)

type Tick = Int
type Register = Int
type ProgramState = (Register, Maybe Int, [Instruction])
type Screen = [String]

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let fns = map drop [19,59..]
    let states = scanl processTick (1, Nothing, input) [1..]
    let rs = ((map getR) . (takeWhile notDone)) states 
    return $ show $ sum $ zipWith (*) [20,60..] $ map head $ takeWhile (\x -> length x > 0) $ map ($ rs) fns

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let states = zipWith (,) [1..] $ takeWhile notDone $ scanl processTick (1, Nothing, input) [1..]
    let initScreen = replicate 6 $ replicate 40 ' '
    return $ unlines $ foldl draw initScreen states

parseInput :: (Monad m) => ParsecT Void String m [Instruction]
parseInput = sepEndBy1 (parseNoop <|> parseAdd) eol
         where parseNoop = do 
                            string "noop"
                            return Noop
               parseAdd = do
                            string "addx "
                            num <- read <$> some (char '-' <|> digitChar)
                            return (AddX num)

processTick :: ProgramState -> Tick -> ProgramState
processTick (r, (Just i), cs) _ = (r+i, Nothing, cs)
processTick (r, _, (Noop:cs)) _ = (r, Nothing, cs)
processTick (r, _, ((AddX i):cs)) _ = (r, Just i, cs)
processTick (r, _, []) _ = (r, Nothing, [])

getR :: ProgramState -> Register
getR (r, _, _) = r

notDone :: ProgramState -> Bool
notDone (_, Nothing, []) = False
notDone _ = True

draw :: Screen -> (Tick, ProgramState) -> Screen
draw s (t, (r, _, _)) = let x = ((t - 1) `div` 40) `mod` 6
                            y = (t - 1) `mod` 40
                            isLit = abs (r - y) < 2
                        in case isLit of
                            True -> setPixel x y '#' s
                            False -> setPixel x y '.' s
                     
setPixel :: Int -> Int -> Char -> Screen -> Screen
setPixel x y c s = let row = s !! x
                       newRow = (take y row) ++ [c] ++ (drop (y + 1) row)
                   in (take x s) ++ [newRow] ++ (drop (x + 1) s)
