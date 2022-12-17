module Problem13 where

import Debug.Trace
import qualified Data.List as L
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Packet = List [Packet] | Val Int
    deriving (Eq)
type PacketPair = (Packet, Packet)

instance Show Packet where
    show (Val a) = show a
    show (List xs) = "[" ++ concat (L.intersperse ", " (map show xs)) ++ "]"

instance Ord Packet where
    compare (Val a) (Val b) | a < b = LT
                            | a > b = GT
                            | otherwise = EQ
    compare (List ps) (Val a) = compare (List ps) (List [Val a])
    compare (Val a) (List ps) = compare (List [Val a]) (List ps)
    compare (List xs) (List ys) = bailFold xs ys
                              where bailFold [] xs = LT
                                    bailFold xs [] = GT
                                    bailFold (x:xs) (y:ys) | compare x y == EQ = bailFold xs ys
                                                           | otherwise = compare x y

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let indexed = zip [1..] input
    return $ show $ sum $ map (\x -> trace (show x) fst x) $ filter (\(i, (a, b)) -> a <= b) indexed

runHard :: FilePath -> IO String
runHard _ = return ""

parseInput :: (Monad m) => ParsecT Void String m [PacketPair]
parseInput = sepEndBy1 parsePair eol
         where parsePair = do
                            p1 <- parsePacket
                            eol
                            p2 <- parsePacket
                            eol
                            return (p1, p2)

parsePacket :: (Monad m) => ParsecT Void String m Packet
parsePacket = do
    char '['
    elements <- sepBy (parseInt <|> parsePacket) (char ',')
    char ']'
    return (List elements)

parseInt :: (Monad m) => ParsecT Void String m Packet
parseInt = do
    i <- read <$> some digitChar
    return (Val i)

