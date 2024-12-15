module Problems.Y2022.Problem13 where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

data Packet = List [Packet] | Val Int
    deriving (Eq)
type PacketPair = (Packet, Packet)

instance Show Packet where
    show (Val a) = show a
    show (List xs) = "[" ++ concat (L.intersperse ", " (map show xs)) ++ "]"

instance Ord Packet where
    compare (Val a) (Val b) = compare a b
    compare (List ps) (Val a) = compare (List ps) (List [Val a])
    compare (Val a) (List ps) = compare (List [Val a]) (List ps)
    compare (List xs) (List ys) = bailFold xs ys
                              where bailFold [] [] = EQ
                                    bailFold [] bs = LT
                                    bailFold as [] = GT
                                    bailFold (a:as) (b:bs) | compare a b == EQ = bailFold as bs
                                                           | otherwise = compare a b

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let indexed = zip [1..] input
    return $ show $ sum $ map fst $ filter (\(i, (a, b)) -> compare a b == LT) indexed

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let allPackets = concat $ map (\(x, y) -> [x,y]) $ input
    return $ show $ product $ map fst $ filter isDivider $ zip [1..] $ L.sort (dividers ++ allPackets)

isDivider :: (Int, Packet) -> Bool
isDivider (_, p) = S.member p $ S.fromList dividers

dividers :: [Packet]
dividers = [List [List [Val 2]], List [List [Val 6]]]

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
    elements <- sepBy (parseIntPacket <|> parsePacket) (char ',')
    char ']'
    return (List elements)

parseIntPacket :: (Monad m) => ParsecT Void String m Packet
parseIntPacket = do
    i <- parseInt
    return (Val i)

