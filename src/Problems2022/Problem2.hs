module Problems2022.Problem2 (run) where

data Throw = R | P | S
data Won = W | L | D
type Game = (Throw, Throw)

runEasy :: String -> String
runEasy = show . sum . map (scoreGame . parseGame) . lines

parseThrow :: Char -> Throw
parseThrow 'A' = R
parseThrow 'X' = R
parseThrow 'B' = P
parseThrow 'Y' = P
parseThrow 'C' = S
parseThrow 'Z' = S
parseThrow _ = error "Invalid throw"

parseGame :: String -> Game
parseGame (t:' ':y:[]) = (parseThrow t, parseThrow y)
parseGame _ = error "Invalid game"

parseGame' :: String -> Game
parseGame' (t:' ':w:[]) = let them = parseThrow t
                              won = parseWon w
                              you = determineYou them won
                          in (them, you)
parseGame' _ = error "Invalid game"

parseWon :: Char -> Won
parseWon 'X' = L
parseWon 'Y' = D
parseWon 'Z' = W
parseWon _ = error "Invalid won"

determineYou :: Throw -> Won -> Throw
determineYou R W = P
determineYou R L = S
determineYou P W = S
determineYou P L = R
determineYou S W = R
determineYou S L = P
determineYou t _ = t

scoreGame :: Game -> Int
scoreGame g@(t, y) = let won = evalWin g
                     in scoreYou y + scoreWon won

scoreYou :: Throw -> Int
scoreYou R = 1
scoreYou P = 2
scoreYou S = 3

evalWin :: Game -> Won
evalWin (R, P) = W
evalWin (R, S) = L
evalWin (P, R) = L
evalWin (P, S) = W
evalWin (S, R) = W
evalWin (S, P) = L
evalWin _ = D

scoreWon :: Won -> Int
scoreWon L = 0
scoreWon D = 3
scoreWon W = 6

run :: String -> String
run = show . sum . map (scoreGame . parseGame') . lines

