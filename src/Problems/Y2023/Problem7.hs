module Problems.Y2023.Problem7 where

import Data.Char
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Debug.Trace

type Hand = (Int, [Card])
type Card = Int
data Rank = High [Int] | One [Int] | Two [Int] | Three [Int] | Full [Int] | Four [Int] | Five [Int] 
    deriving (Eq, Ord, Show)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    let ranked = sortBy (\(_, a) (_, b) -> compare a b) $ map (fmap rank) input
    let bids = map fst ranked
    return $ show $ sum $ zipWith (*) [1..] bids

runHard :: FilePath -> IO String
runHard fp = do
    input <- parseFile parseInput fp
    let ranked = sortBy (\(_, a) (_, b) -> compare a b) $ map ((fmap (upRank . rank)) . replace) input
    let bids = map fst ranked
    return $ show $ sum $ zipWith (*) [1..] bids

replace :: Hand -> Hand
replace (b, cards) = (b, map rep cards)
                 where rep 11 = 1
                       rep x = x

rank :: [Card] -> Rank
rank cards | uniqueCards == 1 = Five cards
           | uniqueCards == 2 = fourOrFull cards
           | uniqueCards == 3 = threeOrTwo cards
           | uniqueCards == 4 = One cards
           | otherwise = High cards
         where uniqueCards = length (nub cards) 

upRank :: Rank -> Rank
upRank r@(Five _) = r
upRank r@(Four cards) | 1 `elem` cards = (Five cards)
                      | otherwise = r
upRank r@(Full cards) | 1 `elem` cards = (Five cards)
                      | otherwise = r
upRank r@(Three cards) | 1 `elem` cards = (Four cards)
                       | otherwise = r
upRank r@(Two cards) | 1 `elem` cards && length (filter (==1) cards) == 2 = (Four cards)
                     | 1 `elem` cards = (Full cards)
                     | otherwise = r
upRank r@(One cards) | 1 `elem` cards = (Three cards)
                     | otherwise = r
upRank r@(High cards) | 1 `elem` cards = (One cards)
                      | otherwise = r

fourOrFull :: [Card] -> Rank
fourOrFull cards = let sorted = sort cards
                       c1 = filter (\c -> c == head cards) cards
                       l = length c1  
                   in case l of
                        1 -> Four cards
                        4 -> Four cards
                        otherwise -> Full cards

threeOrTwo :: [Card] -> Rank
threeOrTwo cards = let sorted = sort cards
                       c1 = filter (\c -> c == head cards) cards
                       c2 = filter (\c -> c == cards !! 4) cards
                   in case (length c1, length c2) of 
                        (3, _) -> Three cards
                        (_, 3) -> Three cards
                        (1, 1) -> Three cards
                        otherwise -> Two cards


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [Hand]
parseInput = sepEndBy1 parseHand eol

parseHand :: (Monad m) => ParsecT Void String m Hand
parseHand = do
    cards <- some $ numberCard <|> faceCard
    char ' '
    bid <- parseInt
    return (bid, cards)

numberCard :: (Monad m) => ParsecT Void String m Int
numberCard = digitToInt <$> digitChar

faceCard :: (Monad m) => ParsecT Void String m Int
faceCard = ten <|> jack <|> queen <|> king <|> ace

ten :: (Monad m) => ParsecT Void String m Int
ten = char 'T' *> pure 10

jack :: (Monad m) => ParsecT Void String m Int
jack = char 'J' *> pure 11

queen :: (Monad m) => ParsecT Void String m Int
queen = char 'Q' *> pure 12

king :: (Monad m) => ParsecT Void String m Int
king = char 'K' *> pure 13

ace :: (Monad m) => ParsecT Void String m Int
ace = char 'A' *> pure 14