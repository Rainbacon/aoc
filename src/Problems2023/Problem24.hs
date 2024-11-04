module Problems2023.Problem24 (runEasy, runHard) where

import Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Velocity = (Float, Float, Float)
type Hailstone = ((Float, Float, Float), Velocity)
-- type Line2D = (Float, Float)
-- type FPoint = (Float, Float)

runEasy :: FilePath -> IO String
runEasy fp = do
  input <- parseFile parseInput fp
  let curves = map (\stone -> (stone, formulate2D stone)) input
  let testRange = (200000000000000, 400000000000000)
  return $ show $ (length $ filter (inTestRange testRange) $ (map findIntersection curves) <*> curves) `div` 2


runHard :: FilePath -> IO String
runHard _ = return ""

-- inTestRange :: Num a => (a, a) -> (Maybe FPoint) -> Bool
inTestRange _ Nothing = False
inTestRange (rl, rh) (Just (a, b, (x, y))) = inRange && (inFuture a (x, y)) && (inFuture b (x, y))
                                         where inRange = rl <= x && x <= rh && rl <= y && y <= rh

-- inFuture :: Hailstone -> FPoint -> Bool
inFuture ((x0, y0, _), (dx, dy, _)) (x, y) = (x - x0) / dx > 0


-- formulate2D :: Hailstone -> Line2D
formulate2D ((x, y, _), (dx, dy, _)) = let slope = dy / dx
                                           intercept = y - (slope *  x)
                                       in (slope, intercept)

-- findIntersection :: Line2D -> Line2D -> (Maybe FPoint)
findIntersection (a, (m1, b1)) (b, (m2, b2)) | m1 == m2 = Nothing
                                             | otherwise = Just (a, b, (x, y))
                                           where x = (b2 - b1) / (m1 - m2)
                                                 y = m1 * x + b1

--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m [Hailstone]
parseInput = sepEndBy1 parseStone eol

parseStone :: (Monad m) => ParsecT Void String m Hailstone
parseStone = do
  positions' <- sepBy1 parseInt (string ", ")
  let positions = map fromIntegral positions'
  string " @ "
  velocities' <- sepBy1 parseInt (string ", ")
  let velocities = map fromIntegral velocities'
  return ((positions !! 0, positions !! 1, positions !! 2), (velocities !! 0, velocities !! 1, velocities !! 2))