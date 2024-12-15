module Problems.Y2023.Problem15 where

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Debug.Trace

data Instruction = Remove String | Replace String Int
    deriving (Show)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFast parseInput fp
    return $ show $ sum $ map (hash 0) input

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput' fp
    let emptyBoxes = replicate 256 []
    let boxes = runLenses input emptyBoxes
    return $ show $ sum $ zipWith power [1..] boxes

power :: Int -> [(String, Int)] -> Int
power n lenses = sum $ zipWith (\(_, l) i -> n * l * i) lenses [1..]

runLenses :: [Instruction] -> [[(String, Int)]] -> [[(String, Int)]]
runLenses [] bs = bs
runLenses ((Remove s):is) bs = runLenses is $ pre ++ [processed] ++ post
                           where boxNum = hash 0 s
                                 pre = take boxNum bs
                                 post = drop (boxNum + 1) bs
                                 cur = head $ drop boxNum bs
                                 processed = filter (\(label, _) -> label /= s) cur
runLenses ((Replace s l):is) bs = runLenses is $ pre ++ [processed] ++ post
                              where boxNum = hash 0 s
                                    pre = take boxNum bs
                                    post = drop (boxNum + 1) bs
                                    cur = head $ drop boxNum bs
                                    processed = addLens (s, l) cur

addLens :: (String, Int) -> [(String, Int)] -> [(String, Int)]
addLens l [] = [l]
addLens lens@(s1, l1) ((s2, l2):ls) | s1 == s2 = lens:ls 
                                    | otherwise = (s2, l2):(addLens lens ls)

hash :: Int -> String -> Int
hash i [] = i 
hash i (x:xs) = hash newI xs 
            where newI = ((i + ord x) * 17) `mod` 256

--- Parsing ---
parseInput :: String -> [String] 
parseInput = splitOn (== ',')

parseInput' :: (Monad m) => ParsecT Void String m [Instruction]
parseInput' = sepEndBy1 parseInstruction (char ',')

parseInstruction :: (Monad m) => ParsecT Void String m Instruction
parseInstruction = do
    label <- some letterChar
    op <- (char '=') <|> (char '-')
    case op of
        '=' -> do
            num <- parseInt
            return $ Replace label num
        '-' -> pure (Remove label)