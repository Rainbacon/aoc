module Problems.Y2022.Problem5 where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

type Piles = M.Map Int [Char]
type Move = (Int, Int, Int)
type InputType = (Piles, [Move])

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return (processInput input (reverse . (uncurry take)))

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    return (processInput input (uncurry take))

processInput :: InputType -> ((Int, [Char]) -> [Char]) -> String
processInput (piles, moves) f = let final = foldl (runMove f) piles moves
                                in map head $ M.elems final

runMove :: ((Int, [Char]) -> [Char]) -> Piles -> Move -> Piles
runMove fn p (n, f, t) = let fromPile = Y.fromJust $ M.lookup f p
                             toPile = Y.fromJust $ M.lookup t p
                             transit = fn (n, fromPile)
                         in M.adjust (transit ++) t $ M.adjust (drop n) f p

parseInput :: (Monad m) => ParsecT Void String m InputType
parseInput = do
              piles <- parsePiles
              eol
              moves <- parseMoves
              return (piles, moves)

parsePiles :: (Monad m) => ParsecT Void String m Piles
parsePiles = do
              rows <- sepEndBy1 parseRow eol
              char ' '
              nums <- sepEndBy1 parseInt (some (char ' '))
              eol
              return (buildPiles rows nums)
              
parseRow :: (Monad m) => ParsecT Void String m [Maybe Char]
parseRow = do
            maybeCrates <- sepEndBy1 (parseCrate <|> parseEmpty) (char ' ')
            return maybeCrates

parseCrate :: (Monad m) => ParsecT Void String m (Maybe Char)
parseCrate = do
              char '['
              c <- letterChar
              char ']'
              return (Just c)

parseEmpty :: (Monad m) => ParsecT Void String m (Maybe Char)
parseEmpty = do
              string "   "
              return Nothing

parseMoves :: (Monad m) => ParsecT Void String m [Move]
parseMoves = some parseMove
         where parseMove = do
                    string "move "
                    n <- parseInt
                    string " from "
                    s <- parseInt
                    string " to "
                    t <- parseInt
                    eol
                    return (n, s, t)

buildPiles :: [[Maybe Char]] -> [Int] -> Piles
buildPiles rows indices = let inf = repeat Nothing
                              infLists = map (\x -> x ++ inf) rows
                          in M.fromList $ map (getCol infLists) indices

reducePile :: [Maybe Char] -> [Char]
reducePile = (map Y.fromJust) . (filter Y.isJust)

getCol :: [[Maybe Char]] -> Int -> (Int, [Char])
getCol infLists i = let pile = map (\x -> x !! (i - 1)) infLists
                    in (i, reducePile pile)
