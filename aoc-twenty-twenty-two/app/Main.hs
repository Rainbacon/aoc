module Main (main) where

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
    let fileName = getFileName day inputType
    (input, handle) <- getInputData fileName
    let problem = Y.fromJust $ M.lookup day problems
    let output = problem input
    putStr output
    hClose handle


getFileName :: String -> String -> String
getFileName day "T" = "data/" ++ day ++ "/test.txt"
getFileName day "R" = "data/" ++ day ++ "/input.txt"
getFileName _ i = error "no input ifle found for option " ++ i

getInputData :: String -> IO (String, Handle)
getInputData fileName = do
                         handle <- openFile fileName ReadMode
                         contents <- hGetContents handle
                         return (contents, handle)

problems ::  M.Map String (String -> String)
problems = M.fromList [("1", P1.run)
                      ,("2", P2.run)
                      ,("3", P3.run)
                      ,("4", P4.run)
                      ,("5", P5.run)
                      ,("6", P6.run)
                      ,("7", P7.run)
                      ,("8", P8.run)
                      ,("9", P9.run)
                      ,("10", P10.run)
                      ,("11", P11.run)
                      ,("12", P12.run)
                      ,("13", P13.run)
                      ,("14", P14.run)
                      ,("15", P15.run)
                      ,("16", P16.run)
                      ,("17", P17.run)
                      ,("18", P18.run)
                      ,("19", P19.run)
                      ,("20", P20.run)
                      ,("21", P21.run)
                      ,("22", P22.run)
                      ,("23", P23.run)
                      ,("24", P24.run)
                      ,("25", P25.run)]
