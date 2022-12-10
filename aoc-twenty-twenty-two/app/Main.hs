module Main (main) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.Map as M
import qualified Data.Maybe as Y

import qualified Problem1 as P1
import qualified Problem2 as P2
import qualified Problem3 as P3
import qualified Problem4 as P4
import qualified Problem5 as P5
import qualified Problem6 as P6
import qualified Problem7 as P7
import qualified Problem8 as P8
import qualified Problem9 as P9
import qualified Problem10 as P10
import qualified Problem11 as P11
import qualified Problem12 as P12
import qualified Problem13 as P13
import qualified Problem14 as P14
import qualified Problem15 as P15
import qualified Problem16 as P16
import qualified Problem17 as P17
import qualified Problem18 as P18
import qualified Problem19 as P19
import qualified Problem20 as P20
import qualified Problem21 as P21
import qualified Problem22 as P22
import qualified Problem23 as P23
import qualified Problem24 as P24
import qualified Problem25 as P25

main :: IO ()
main = do
    putStrLn "What day would you like to run?"
    day <- getLine
    putStrLn "Do you want to (T)est or (R)un the code?"
    inputType <- getLine
    putStrLn "Would you like to run the (E)asy or (H)ard version?"
    problemType <- getLine
    let fileName = getFileName day inputType
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
getFileName day "T" = "data/" ++ day ++ "/test.txt"
getFileName day "R" = "data/" ++ day ++ "/input.txt"
getFileName _ i = error "no input ifle found for option " ++ i

getInputData :: String -> IO (String, Handle)
getInputData fileName = do
                         handle <- openFile fileName ReadMode
                         contents <- hGetContents handle
                         return (contents, handle)

problemsEasy :: M.Map String (FilePath -> IO String)
problemsEasy = M.fromList [("1", P1.runEasy)
                          --,("2", P2.runEasy)
                          --,("3", P3.runEasy)
                          --,("4", P4.runEasy)
                          ,("5", P5.runEasy)
                          ,("6", P6.runEasy)
                          ,("7", P7.runEasy)
                          --,("8", P8.runEasy)
                          --,("9", P9.runEasy)
                          --,("10", P10.runEasy)
                          --,("11", P11.runEasy)
                          --,("12", P12.runEasy)
                          --,("13", P13.runEasy)
                          --,("14", P14.runEasy)
                          --,("15", P15.runEasy)
                          --,("16", P16.runEasy)
                          --,("17", P17.runEasy)
                          --,("18", P18.runEasy)
                          --,("19", P19.runEasy)
                          --,("20", P20.runEasy)
                          --,("21", P21.runEasy)
                          --,("22", P22.runEasy)
                          --,("23", P23.runEasy)
                          --,("24", P24.runEasy)
                          --,("25", P25.runEasy)
                          ]

problemsHard :: M.Map String (String -> IO String)
problemsHard = M.fromList [("1", P1.runHard)
                          --,("2", P2.runHard)
                          --,("3", P3.runHard)
                          --,("4", P4.runHard)
                          ,("5", P5.runHard)
                          ,("6", P6.runHard)
                          --,("7", P7.runHard)
                          --,("8", P8.runHard)
                          --,("9", P9.runHard)
                          --,("10", P10.runHard)
                          --,("11", P11.runHard)
                          --,("12", P12.runHard)
                          --,("13", P13.runHard)
                          --,("14", P14.runHard)
                          --,("15", P15.runHard)
                          --,("16", P16.runHard)
                          --,("17", P17.runHard)
                          --,("18", P18.runHard)
                          --,("19", P19.runHard)
                          --,("20", P20.runHard)
                          --,("21", P21.runHard)
                          --,("22", P22.runHard)
                          --,("23", P23.runHard)
                          --,("24", P24.runHard)
                          --,("25", P25.runHard)
                          ]
