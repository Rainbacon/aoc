module Main (main) where

import Control.Monad.IO.Class
import Options.Applicative (execParser)
import System.IO
import System.CPUTime
import Text.Printf
import qualified Data.Map as M
import qualified Data.Maybe as Y

import AOCTypes (ProblemSet)
import Args
import qualified Problems2022 as P2022
import qualified Problems2023 as P2023


main :: IO ()
main = do
    options <- execParser opts
    let year = optYear options
    let day = optDay options
    let inputType = optMode options
    let problemType = optPart options
    let fileName = getFileName year day ++ show inputType
    let (problemsEasy, problemsHard) = Y.fromJust $ M.lookup year allProblems
    case problemType of
        EasyProblem -> do
                        let problem = Y.fromJust $ M.lookup day problemsEasy
                        start <- getCPUTime
                        output <- problem fileName
                        end <- getCPUTime
                        putStrLn $ "Answer: " ++ output
                        let diff = (fromIntegral (end - start)) / (10^12)
                        printf "Computation time: %0.3f sec\n" (diff :: Double)
        HardProblem -> do
                        let problem = Y.fromJust $ M.lookup day problemsHard
                        start <- getCPUTime
                        output <- problem fileName
                        end <- getCPUTime
                        putStrLn $ "Answer: " ++ output
                        let diff = (fromIntegral (end - start)) / (10^12)
                        printf "Computation time: %0.3f sec\n" (diff :: Double)


getFileName :: String -> String -> String
getFileName year day = "data/" ++ year ++ "/" ++ day ++ "/"

getInputData :: String -> IO (String, Handle)
getInputData fileName = do
                         handle <- openFile fileName ReadMode
                         contents <- hGetContents handle
                         return (contents, handle)


allProblems :: M.Map String (ProblemSet, ProblemSet)
allProblems = M.fromList [("2022", P2022.problems)
                         ,("2023", P2023.problems)
                         ]