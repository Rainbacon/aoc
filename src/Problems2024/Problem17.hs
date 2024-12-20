module Problems2024.Problem17 (runEasy, runHard) where

import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import Data.Bits
import Debug.Trace
import Data.List

type Computer = (Int, Int, Int, Int)
type Combo = Int
type Lit = Int
data Instruction = Adv Combo | Bxl Lit | Bst Combo | Jnz Lit | Bxc Lit | Out Combo | Bdv Combo | Cdv Combo
    deriving (Show)

toInt :: Instruction -> [Int]
toInt (Adv a) = [0,a]
toInt (Bxl a) = [1,a]
toInt (Bst a) = [2,a]
toInt (Jnz a) = [3,a]
toInt (Bxc a) = [4,a]
toInt (Out a) = [5,a]
toInt (Bdv a) = [6,a]
toInt (Cdv a) = [7,a]


runEasy :: FilePath -> IO String
runEasy fp = do
    (computer, instructions) <- parseFile parseInput fp
    putStrLn $ show instructions
    return $ show $ ST.evalState (runProgram instructions) computer

runProgram :: [Instruction] -> ST.State Computer [Int] 
runProgram instructions = do
    comp@(_, _, _, index) <- ST.get
    if index >= (length instructions)
    then return []
    else do
        let (newComp, out) = processInstruction comp (instructions !! index)
        ST.put newComp
        if Y.isJust out
        then do
            rest <- runProgram instructions
            let this = Y.fromJust out
            return $ (this:rest)
        else do 
            rest <- runProgram instructions
            return rest 

processInstruction :: Computer -> Instruction -> (Computer, Maybe Int)
processInstruction comp@(a, b, c, i) (Adv f) = ((a `div` (2 ^ p), b, c, i + 1), Nothing)
    where p = [0,1,2,3,a,b,c] !! f
processInstruction comp@(a, b, c, i) (Bxl d) = ((a, xor b d, c, i + 1), Nothing)
processInstruction comp@(a, b, c, i) (Bst f) = ((a, divisor `mod` 8, c, i + 1), Nothing)
    where divisor = [0,1,2,3,a,b,c] !! f
processInstruction comp@(a, b, c, i) (Jnz newI) = if a == 0 then ((a, b, c, i + 1), Nothing) else ((a, b, c, newI `div` 2), Nothing)
processInstruction comp@(a, b, c, i) (Bxc _) = ((a, xor b c, c, i + 1), Nothing)
processInstruction comp@(a, b, c, i) (Out f) = ((a, b, c, i + 1), Just (divisor `mod` 8))
    where divisor = [0,1,2,3,a,b,c] !! f
processInstruction comp@(a, b, c, i) (Bdv f) = ((a, a `div` (2 ^ divisor), c, i + 1), Nothing)
    where divisor = [0,1,2,3,a,b,c] !! f
processInstruction comp@(a, b, c, i) (Cdv f) = ((a, b, a `div` (2 ^ divisor), i + 1), Nothing)
    where divisor = [0,1,2,3,a,b,c] !! f

runHard :: FilePath -> IO String
runHard fp = do
    (computer, instructions) <- parseFile parseInput fp
    let target = concatMap toInt instructions
        working = runReverse' (reverse target) (0, 0, 0, 0)
    return $ show $ head $ sortBy (\(a,_,_,_) (b,_,_,_) -> compare a b) working

runReverse' :: [Int] -> Computer -> [Computer]
runReverse' [] c = [c]
runReverse' (x:xs) c = concatMap (runReverse' xs) states
    where states = runReverse c x

runComputer :: [Int] -> Computer -> [Instruction] -> Int
runComputer target comp@(a, b, c, i) ins | output == target = a
                                         | otherwise = runComputer target (a + 1, b, c, i) ins
    where output = ST.evalState (runProgram ins) comp

runReverse :: Computer -> Int -> [Computer]
runReverse (a, b, c, i) n = map (\(pb, pc) -> (shiftedA + (pb `xor` 2), pb, pc, i)) actualBC
    where finalB = n
          xor7 = finalB `xor` 7
          shiftedA = shift a 3
          possibleFirstB = map (xor 2) [0..7]
          possibleBC = map (\pb -> (pb, (shiftedA + (pb `xor` 2)) `div` (2 ^ pb))) possibleFirstB
          actualBC = filter (\(pb, pc) -> ((pb `xor` pc) `mod` 8) == xor7) possibleBC

parseInput :: (Monad m) => ParsecT Void String m (Computer, [Instruction])
parseInput = do
    _ <- string "Register A: "
    a <- parseInt
    _ <- eol
    _ <- string "Register B: "
    b <- parseInt
    _ <- eol
    _ <- string "Register C: "
    c <- parseInt
    _ <- eol
    _ <- eol
    _ <- string "Program: "
    ops <- sepBy1 parseOperand (char ',')
    return ((a,b,c, 0), ops)

parseOperand :: (Monad m) => ParsecT Void String m Instruction
parseOperand = do
    ins <- parseInt
    _ <- char ','
    op <- parseInt
    return $ parseOperand' ins op

parseOperand' :: Int -> Int -> Instruction
parseOperand' i op =
    case i of
        0 -> Adv op
        1 -> Bxl op
        2 -> Bst op
        3 -> Jnz op
        4 -> Bxc op
        5 -> Out op
        6 -> Bdv op
        7 -> Cdv op
