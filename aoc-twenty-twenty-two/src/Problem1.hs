module Problem1 (run) where

import Data.List
import Text.Parsec

type Elf = Int

run :: String -> String
run input = show $ sum . (take 3) . reverse . sort $ parseInput input

parseInput :: String -> [Elf]
parseInput input = let lns = lines input
                   in parseElves lns

parseElves :: [String] -> [Elf]
parseElves [] = []
parseElves xs = let elf = takeWhile (/= "") xs
                    rest = dropWhile (/= "") xs
                in case rest of
                    [] -> [parseElf elf]
                    _ -> (parseElf elf):(parseElves $ tail rest)

parseElf :: [String] -> Elf
parseElf = sum . (map read)
