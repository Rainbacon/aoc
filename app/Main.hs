module Main (main) where

import Options.Applicative (execParser)
import System.CPUTime
import Text.Printf
import qualified Data.Map as M
import qualified Data.Maybe as Y

import AOCTypes (ProblemSet)
import Args
import Input
import qualified Problems.Y2022 as P2022
import qualified Problems.Y2023 as P2023
import qualified Problems.Y2024 as P2024


main :: IO ()
main = do
    options <- execParser opts
    let year = optYear options
    let day = optDay options
    let inputType = optMode options
    let problemType = optPart options
    let fileName = getFileName year day ++ show inputType
    let (problemsEasy, problemsHard) = Y.fromJust $ M.lookup year allProblems
    loadInput options
    case problemType of
        EasyProblem -> do
                        let problem = Y.fromJust $ M.lookup day problemsEasy
                        start <- getCPUTime
                        output <- problem fileName
                        putStrLn $ "Answer: " ++ output
                        end <- getCPUTime
                        let diff = (fromIntegral (end - start)) / (1000000000000)
                        printf "Computation time: %0.3f sec\n" (diff :: Double)
        HardProblem -> do
                        let problem = Y.fromJust $ M.lookup day problemsHard
                        start <- getCPUTime
                        output <- problem fileName
                        putStrLn $ "Answer: " ++ output
                        end <- getCPUTime
                        let diff = (fromIntegral (end - start)) / (1000000000000)
                        printf "Computation time: %0.3f sec\n" (diff :: Double)


getFileName :: String -> String -> String
getFileName year day = "data/" ++ year ++ "/" ++ day ++ "/"

allProblems :: M.Map String (ProblemSet, ProblemSet)
allProblems = M.fromList [("2022", P2022.problems)
                         ,("2023", P2023.problems)
                         ,("2024", P2024.problems)
                         ]