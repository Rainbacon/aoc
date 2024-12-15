module Problems.Y2023.Problem3 (runEasy, runHard) where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing
import Debug.Trace

data Part = Symbol Char | Number Int
type PartLength = (Int, Int, Part)
type PartList = [PartLength]

isNumber :: Part -> Bool
isNumber (Symbol _) = False
isNumber (Number _) = True

isSymbol :: Part -> Bool
isSymbol = not . isNumber

runEasy :: FilePath -> IO String
runEasy fs = do
              (parts, symbolMap) <- parseFile parseInput fs
              let flattened = concat $ map distribute parts
              return $ show $ sum $ map (extractNum' . snd) $ filter (validPart symbolMap) flattened

runHard :: FilePath -> IO String
runHard fs = do
              (parts, symbolMap) <- parseFile parseInput fs
              let gears = M.filter isGear symbolMap
              let flattenedParts = concat $ map distribute parts
              let gearParts = map (findParts flattenedParts) (M.assocs gears)
              return $ show $ sum $ map gearPower $ filter ((== 2) . length) gearParts

isGear :: Part -> Bool
isGear (Number _) = False
isGear (Symbol s) = s == '*'

gearPower :: [Part] -> Int
gearPower = product . (map extractNum)

findParts :: [(Int, PartLength)] -> (Point, Part) -> [Part]
findParts parts (point, gear) = let m = M.singleton point gear
                                in map (\(_, (_, _, p)) -> p) $ filter (validPart m) parts

validPart :: M.Map Point Part -> (Int, PartLength) -> Bool
validPart symbolMap (x, (a, b, _)) = let path = [a - 1..b + 1] 
                                         points = (map (,) [x-1..x+1]) <*> path
                                         foundSymbols = map (\p -> M.lookup p symbolMap) points
                                      in length (filter Y.isJust foundSymbols) > 0

extractNum' :: PartLength -> Int
extractNum' (_, _, p) = extractNum p

extractNum :: Part -> Int
extractNum (Symbol _) = 0
extractNum (Number i) = i


--- Parsing ---
parseInput :: (Monad m) => ParsecT Void String m ([(Int, PartList)], M.Map Point Part)
parseInput = do 
              partLists <- sepEndBy1 parseLine eol
              let numbers = map (filter hasNumber) partLists
              let symbols = map (filter hasSymbol) partLists
              return (zip [0..] numbers, buildSymbols symbols)

hasNumber :: (Int, Int, Part) -> Bool
hasNumber (_, _, p) = isNumber p

hasSymbol :: (Int, Int, Part) -> Bool
hasSymbol (_, _, p) = isSymbol p

buildSymbols :: [PartList] -> M.Map Point Part
buildSymbols ps = let indexed = zip [0..] ps
                      flattened = concat $ map flattener indexed
                      flattener (x, list) = map (\(y, _, p) -> ((x, y), p)) list
                  in M.fromList flattened

parseLine :: (Monad m) => ParsecT Void String m PartList
parseLine = do
             results <- many $ parseNothing <|> parsePart
             return $ constructLineParts 0 results

constructLineParts :: Int -> [(Int, Maybe Part)] -> PartList
constructLineParts _ [] = []
constructLineParts i ((l, Nothing):ps) = constructLineParts (i + l) ps
constructLineParts i ((l, Just p):ps) = (i, i + l - 1, p):(constructLineParts (i + l) ps)

parseNothing :: (Monad m) => ParsecT Void String m (Int, Maybe Part)
parseNothing = do
                garbage <- some $ char '.'
                return $ (length garbage, Nothing)

parsePart :: (Monad m) => ParsecT Void String m (Int, Maybe Part)
parsePart = parseNumber <|> parseSymbol

parseNumber :: (Monad m) => ParsecT Void String m (Int, Maybe Part)
parseNumber = do
               num <- some digitChar
               return $ (length num, Just (Number (read num)))

parseSymbol :: (Monad m) => ParsecT Void String m (Int, Maybe Part)
parseSymbol = do
               c <- oneOf ['!', '@','#','$','%','^','&','*','-','+','=','/','?','>','<',',','{','}','[',']','(',')','|']
               return $ (1, Just $ Symbol c)