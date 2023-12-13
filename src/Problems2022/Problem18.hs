module Problems2022.Problem18 (runEasy, runHard) where

import qualified Control.Monad.State as ST
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils
import Debug.Trace

type Drops = S.Set Point3D
data Bounds = Bounds {
    maxX :: Int
  , minX :: Int
  , maxY :: Int
  , minY :: Int
  , maxZ :: Int
  , minZ :: Int
} 

runEasy :: FilePath -> IO String
runEasy fp = do
    drops <- parseFile parseInput fp
    return $ show $ sum $ map (countOpen drops) (S.toList drops)

runHard :: FilePath -> IO String
runHard fp = do
    drops <- parseFile parseInput fp
    let maxX' = maximum $ S.map fst3 drops
    let minX' = minimum $ S.map fst3 drops
    let maxY' = maximum $ S.map snd3 drops
    let minY' = minimum $ S.map snd3 drops
    let maxZ' = maximum $ S.map thd3 drops
    let minZ' = minimum $ S.map thd3 drops
    let bounds = (Bounds maxX' minX' maxY' minY' maxZ' minZ')
    let filledDrops = ST.execState (runBubbles bounds) drops
    putStrLn $ show $ S.toList filledDrops
    return $ show $ sum $ map (countOpen filledDrops) (S.toList filledDrops)

runBubbles :: Bounds -> ST.State Drops ()
runBubbles b@(Bounds xx nx xy ny xz nz) = do
    let points = [(x, y, z) | x <- [nx..xx], y <- [ny..xy], z <- [nz..xz]]
    mapM_ (runBubble b) points

runBubble :: Bounds -> Point3D -> ST.State Drops ()
runBubble bounds p = do
    drops <- ST.get
    let bubble = expandBubble drops S.empty bounds p
    let fullBubble = concatMaybe ((Just [p]):[bubble])
    case fullBubble of
        (Just xs) -> ST.put $ S.union drops (S.fromList xs)
        Nothing -> return ()

countOpen' :: Drops -> Bounds -> Point3D -> Int
countOpen' drops bounds point = length $ filter Y.isNothing bubbles 
                            where adjacent = neighbors point
                                  potentials = filter (\p -> not $ S.member p drops) adjacent
                                  bubbles = map (expandBubble drops S.empty bounds) potentials

concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe [] = Just []
concatMaybe (Nothing:_) = Nothing
concatMaybe ((Just x):xs) = fmap (\a -> x ++ a) (concatMaybe xs)

expandBubble :: Drops -> Drops -> Bounds -> Point3D -> (Maybe [Point3D]) 
expandBubble allDrops seen bounds point | S.member point allDrops = (Just [])
                                        | S.member point seen = (Just [point])
                                        | outOfBounds bounds point = Nothing
                                        | otherwise = concatMaybe $ map (expandBubble allDrops (S.insert point seen) bounds) (neighbors point)

outOfBounds :: Bounds -> Point3D -> Bool
outOfBounds bounds (x, y, z) = x >= (maxX bounds) || x <= (minX bounds) || y >= (maxY bounds) || y <= (minY bounds) || z >= (maxZ bounds) || z <= (minZ bounds)

countOpen :: Drops -> Point3D -> Int
countOpen drops point = length $ filter (\p -> not $ S.member p drops) (neighbors point)
                        
neighbors :: Point3D -> [Point3D]
neighbors (x, y, z) = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1)]

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m Drops
parseInput = do
    points <- sepEndBy1 parse3Point eol
    return $ S.fromList points

parse3Point :: (Monad m) => ParsecT Void String m Point3D
parse3Point = do 
    parts <- sepEndBy1 parseInt (char ',')
    return (parts !! 0, parts !! 1, parts !! 2)