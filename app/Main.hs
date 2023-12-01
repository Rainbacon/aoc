module Main (main) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.Map as M
import qualified Data.Maybe as Y

import AOCTypes (ProblemSet)
import qualified Problems2022 as P2022
import qualified Problems2023 as P2023


main :: IO ()
main = do
    putStrLn "What year would you like to run?"
    year <- getLne
    putStrLn "What day would you like to run?"
    day <- getLine
    putStrLn "Do you want to (T)est or (R)un the code?"
    inputType <- getLine
    putStrLn "Would you like to run the (E)asy or (H)ard version?"
    problemType <- getLine
    let fileName = getFileName day inputType
    let (problemsEasy, problemsHard) = Y.fromJust $ M.lookup year allProblems
    case problemType of
        "E" -> do
                let problem = Y.fromJust $ M.lookup day problemsEasy
                output <- problem fileName
                putStrLn output
        "H" -> do
                let problem = Y.fromJust $ M.lookup day problemsHard
                output <- problem fileName
                putStrLn output


getFileName :: String -> String -> String
getFileName day "T" = "data/" ++ year ++ "/" ++ day ++ "/test.txt"
getFileName day "R" = "data/" ++ year ++ "/" ++ day ++ "/input.txt"
getFileName _ i = error "no input ifle found for option " ++ i

getInputData :: String -> IO (String, Handle)
getInputData fileName = do
                         handle <- openFile fileName ReadMode
                         contents <- hGetContents handle
                         return (contents, handle)


allProblems :: M.Map String (ProblemSet, ProblemSet)
allProblems = M.fromList [("2022", P2022.problems)
                         ,("2023", P2023.problems)
                         ]