module Problem5 (runEasy) where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Piles = M.Map Int ([Char])
type Move = (Int, Int, Int)
type InputType = (Piles, [Move])

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return (show $ fst input)

parseInput :: (Monad m) => ParsecT Void String m InputType
parseInput = do
              piles <- parsePiles
              eol
              moves <- parseMoves
              return (piles, moves)

parsePiles :: (Monad m) => ParsecT Void String m Piles
parsePiles = do
              rows <- many parseRow
              spaceChar
              nums <- sepBy parseInt (some spaceChar)
              eol
              return (buildPiles rows nums)
              
parseRow :: (Monad m) => ParsecT Void String m [Maybe Char]
parseRow = do
            maybeCrates <- sepBy (parseCrate <|> parseEmpty) spaceChar 
            eol
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

parseInt :: (Monad m) => ParsecT Void String m Int
parseInt = read <$> some digitChar

buildPiles :: [[Maybe Char]] -> [Int] -> Piles
buildPiles rows indices = let inf = repeat Nothing
                              infLists = map (\x -> x ++ inf) rows
                          in M.fromList $ map (getCol infLists) indices

reducePile :: [Maybe Char] -> [Char]
reducePile = (map Y.fromJust) . (filter Y.isJust)

getCol :: [[Maybe Char]] -> Int -> (Int, [Char])
getCol infLists i = let pile = map (\x -> x !! i) infLists
                    in (i, reducePile pile)
