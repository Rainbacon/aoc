module Problems2024.Problem24 (runEasy, runHard) where

import Utils.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Bits
import Data.List
import qualified Data.Map as M
import Text.Printf
import Debug.Trace

data Wire = Initial Bool | AND String String | OR String String | XOR String String
    deriving (Show)

instance Eq Wire where
    (==) (Initial a) (Initial b) = a == b
    (==) (AND a b) (AND c d) = (a == c && b == d) || (a == d && b == c)
    (==) (OR a b) (OR c d) = (a == c && b == d) || (a == d && b == c)
    (==) (XOR a b) (XOR c d) = (a == c && b == d) || (a == d && b == c)
    (==) _ _ = False

comboOf :: String -> String -> Wire -> Bool
comboOf _ _ (Initial _) = False
comboOf a b w = (a == c && b == d) || (a == d && b == c)
    where (c, d) = extract w
          extract (AND x y) = (x, y)
          extract (OR x y) = (x, y)
          extract (XOR x y) = (x, y)

isGate :: Wire -> Bool
isGate (Initial _) = False
isGate _ = True

isXOR :: Wire -> Bool
isXOR (XOR _ _) = True
isXOR _ = False

isAND :: Wire -> Bool
isAND (AND _ _) = True
isAND _ = False

isOR :: Wire -> Bool
isOR (OR _ _) = True
isOR _ = False

runEasy :: FilePath -> IO String
runEasy fp = do
    wires <- parseFile parseInput fp
    let wiresToRun = sort $ filter (\s -> "z" `isPrefixOf` s) $ M.keys wires
        wiresRun = zip [0..] $ map (runWire wires) wiresToRun
    return $ show $ foldl condenseWire 0 wiresRun

condenseWire :: Int -> (Int, Bool) -> Int
condenseWire n (i, False) = clearBit n i
condenseWire n (i, True) = setBit n i

runWire :: M.Map String Wire -> String -> Bool
runWire wires s = let wire = wires M.! s
    in case wire of
        (Initial b) -> b 
        (AND a b) -> (runWire wires a) && (runWire wires b)
        (OR a b) -> (runWire wires a) || (runWire wires b)
        (XOR a b) -> (runWire wires a) `xor` (runWire wires b)

data Adder = Adder {
    inputBits :: [String]
  , outputBit :: String
  , carryOut :: Maybe String
  , broken :: [String]
}

runHard :: FilePath -> IO String
runHard fp = do
    wires <- parseFile parseInput fp
    let wiresToRun = sort $ filter (\s -> "z" `isPrefixOf` s) $ M.keys wires
        mapping = foldl M.union M.empty $ map (buildMapping wires) wiresToRun 
        reverseMapping = foldl (M.unionWith (++)) M.empty $ map (\w -> buildReverseMapping wires (w, read (tail w))) wiresToRun
        bothMappings = M.intersectionWith (,) mapping reverseMapping
    return $ unlines $ map printMapping $ M.assocs bothMappings

printMapping :: (String, ([Int], [Int])) -> String
printMapping (gate, (inputs, outputs)) = "Gate " ++ gate ++ ":\nInputs: " ++ show (nub (sort inputs)) ++ "\nOutputs: " ++ show (nub (sort outputs)) 

buildReverseMapping :: M.Map String Wire -> (String, Int) -> M.Map String [Int]
buildReverseMapping wires (wire, parent) | not (isGate mapping) = M.singleton wire [parent]
                                         | otherwise = M.insertWith (++) wire [parent] restOfMap
    where mapping = wires M.! wire
          extract (Initial _) = []
          extract (AND a b) = [a,b]
          extract (OR a b) = [a,b]
          extract (XOR a b) = [a,b]
          children = extract mapping
          restOfMap = foldl (M.unionWith (++)) M.empty $ map (\kid -> buildReverseMapping wires (kid, parent)) children

buildMapping :: M.Map String Wire -> String -> M.Map String [Int]
buildMapping wires wire | not (isGate mapping) = M.singleton wire [read (tail wire)]
                        | otherwise = M.insert wire inputs restOfMap
    where mapping = wires M.! wire
          extract (Initial _) = []
          extract (AND a b) = [a,b]
          extract (OR a b) = [a,b]
          extract (XOR a b) = [a,b]
          children = extract mapping
          restOfMap = foldl M.union M.empty $ map (buildMapping wires) children 
          inputs = concatMap (\kid -> restOfMap M.! kid) children
--     let middleBits = [1..44]
--         c0 = head $ M.keys $ M.filter (== (AND "x00" "y00")) wires
--         a0 = Adder {
--             inputBits = ["x00", "y00"],
--             outputBit = "z00",
--             carryOut = Just c0,
--             broken = []
--         }

-- constructAdders :: M.Map String Wire -> Int -> [Adder] -> [Adder]
-- constructAdders wires 45 as = as
-- constructAdders wires n as@(a:_) = constructAdders wires (n+1) (newA:as)
--     where newA = Adder {
--             inputBits = inBits,
--             outputBit = printf "z%02d" n,
--             carryOut = Just carry,
--             broken = []
--           }
--           inBits = findBits n,
--           carryIn = Y.fromJust $ carryOut a
--           firstLine = M.assocs $ M.filter (comboOf (inBits !! 0) (inBits !! 1)) wires
--           (intermediateAdd, intermediateCarry) = rectify firstLines

-- rectify :: [(String, Wire)] -> ((String, Bool), (String, Bool))
-- rectify 

-- findBits :: Int -> [String]
-- findBits n = [printf "x%02d" n, printf "y%02d" n]

parseInput :: (Monad m) => ParsecT Void String m (M.Map String Wire)
parseInput = do
    wires <- sepEndBy1 parseLiteral eol
    _ <- eol
    gates <- sepEndBy1 parseGate eol
    return $ M.union (M.fromList wires) (M.fromList gates)


parseLiteral :: (Monad m) => ParsecT Void String m (String, Wire)
parseLiteral = do
    name <- many alphaNumChar
    _ <- string ": "
    val <- (char '0' *> pure False) <|> (char '1' *> pure True)
    return (name, Initial val)

parseGate :: (Monad m) => ParsecT Void String m (String, Wire)
parseGate = do
    i1 <- many alphaNumChar
    _ <- spaceChar
    constructor <- (string "AND" *> pure (AND)) <|> (string "OR" *> pure (OR)) <|> (string "XOR" *> pure (XOR))
    _ <- spaceChar
    i2 <- many alphaNumChar
    _ <- string " -> "
    res <- many alphaNumChar
    return (res, constructor i1 i2)