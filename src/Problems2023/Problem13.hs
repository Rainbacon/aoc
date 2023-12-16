module Problems2023.Problem13 where

import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils
import Debug.Trace

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let refH = map ((100*) . sum . (reflect [])) input
    let refV = map (sum . (reflect []) . transpose) input
    return $ show $ (sum refH) + (sum refV)

runHard :: FilePath -> IO String
runHard fp = do 
    input <- parseFile parseInput fp
    return $ show $ sum $ map reflect' input

reflect :: [String] -> [String] -> [Int]
reflect _ [] = []
reflect [] (y:ys) = reflect [y] ys
reflect xs ys@(y:yr) | mirror = (length xs):(reflect (y:xs) yr)
                     | otherwise = reflect (y:xs) yr
                   where mirror = all id $ zipWith (==) xs ys


reflect' :: [String] -> Int
reflect' xs = 100 * (sum diffH') + (sum diffV')
          where possibles = concat $ zipWith (\x row -> zipWith (\y _ -> flipAt x y xs) [0..] row) [0..] xs
                refH = reflect [] xs
                refV = reflect [] $ transpose xs
                reflectionsH = map (reflect []) possibles
                reflectionsV = map ((reflect []) . transpose) possibles
                diffH = filter (/= refH) reflectionsH
                diffH' = nub $ concat $ map (\\ refH) diffH 
                diffV = filter (/= refV) reflectionsV
                diffV' = nub $ concat $ map (\\ refV) diffV 

flipAt :: Int -> Int -> [String] -> [String]
flipAt x y ss = let c = head $ drop y row
                    preRows = take x ss
                    row = head $ drop x ss
                    postRows = drop (x + 1) ss
                    preCols = take y row
                    postCols = drop (y + 1) row
               in case c of
                '.' -> preRows ++ [(preCols ++ ['#'] ++ postCols)] ++ postRows
                '#' -> preRows ++ [(preCols ++ ['.'] ++ postCols)] ++ postRows
                x -> preRows ++ [(preCols ++ [x] ++ postCols)] ++ postRows


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [[String]]
parseInput = sepEndBy1 parseGrid eol

parseGrid :: (Monad m) => ParsecT Void String m [String]
parseGrid = sepEndBy1 (some $ (char '#' *> pure '#') <|> (char '.' *> pure '.')) eol