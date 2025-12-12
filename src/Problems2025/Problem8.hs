module Problems2025.Problem8 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import qualified Data.Set as S
import qualified Control.Monad.State as ST

type Junction = (Int, Int, Int)
type JunctionState = [S.Set Junction]
 
toJunction :: [Int] -> Junction
toJunction xs = (xs !! 0, xs !! 1, xs !! 2)

runEasy :: FilePath -> IO String
runEasy fp = do
    junctions <- parseFile parseJunctions fp
    putStrLn "How many pairs?"
    numPairs <- read <$> getLine
    let pairs = take (2*numPairs) $ sortOn (uncurry dist) [(a, b) | a <- junctions, b <- junctions, a /= b]
    return $ show $ product $ take 3 $ reverse $ sort $ map S.size $ ST.execState (mapM_ chainJunction pairs) []

chainJunction :: (Junction, Junction) -> ST.State JunctionState (Int, (Junction, Junction))
chainJunction (a, b) = do
    chains <- ST.get
    let aChain = findChain a chains
    let bChain = findChain b chains
    let rest = filter (\c -> S.notMember a c && S.notMember b c) chains
    let merged = mergeChains aChain bChain (a, b)
    ST.put (merged:rest)
    return (S.size merged, (a, b))

mergeChains :: Maybe (S.Set Junction) -> Maybe (S.Set Junction) -> (Junction, Junction) -> S.Set Junction
mergeChains Nothing Nothing (a, b) = S.fromList [a, b]
mergeChains (Just c) Nothing (_, b) = S.insert b c
mergeChains Nothing (Just c) (a, _) = S.insert a c
mergeChains (Just c) (Just d) _ = S.union c d

findChain :: Junction -> [S.Set Junction] -> Maybe (S.Set Junction)
findChain j chains | length found == 0 = Nothing
                   | otherwise = Just $ head found
                 where found = filter (S.member j) chains

dist :: Floating a => Junction -> Junction -> a
dist (x1, y1, z1) (x2, y2, z2) = sqrt $ square (x1 - x2) + square (y1 - y2) + square (z1 - z2)

square :: Floating a => Int -> a
square = fromIntegral . (^2)

runHard :: FilePath -> IO String
runHard fp = do
    junctions <- parseFile parseJunctions fp
    let pairs = sortOn (uncurry dist) [(a, b) | a <- junctions, b <- junctions, a /= b]
    return $ show $ (\(_, ((x1, _, _), (x2, _, _))) -> x1 * x2) $ head $ dropWhile (\(s, _) -> s < length junctions) $ ST.evalState (mapM chainJunction pairs) []

parseJunctions :: (Monad m) => ParsecT Void String m [Junction]
parseJunctions = sepEndBy1 parseJunction eol

parseJunction :: (Monad m) => ParsecT Void String m Junction
parseJunction = do
    coords <- sepBy1 parseInt (char ',')
    return $ toJunction coords