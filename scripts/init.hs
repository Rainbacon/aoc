#! /usr/bin/env stack

import System.Directory
import System.IO

main :: IO ()
main = do
    putStrLn "What is the year?"
    year <- getLine
    putStrLn $ "Initializing " ++ year ++ "..."
    let dirName = "src/Problems" ++ year
    createDirectory dirName
    mapM_ (createDayFile dirName year) [1..25]
    createYearFile year

createDayFile :: FilePath -> String -> Int -> IO ()
createDayFile fp year day = do
    let fileName = fp ++ "/Problem" ++ show day ++ ".hs"
    handle <- openFile fileName WriteMode
    hPutStrLn handle ("module Problems" ++ year ++ ".Problem" ++ (show day) ++ " (runEasy, runHard) where" )
    hPutStrLn handle ""
    hPutStrLn handle "runEasy :: FilePath -> IO String"
    hPutStrLn handle "runEasy _ = return \"\""
    hPutStrLn handle ""
    hPutStrLn handle "runHard :: FilePath -> IO String"
    hPutStrLn handle "runHard _ = return \"\""
    hClose handle

createYearFile :: String -> IO ()
createYearFile year = do
    let fileName = "src/Problems" ++ year ++ ".hs"
    handle <- openFile fileName WriteMode
    hPutStrLn handle ("module Problems" ++ year ++ " (problems) where")
    hPutStrLn handle ""
    hPutStrLn handle "import qualified Data.Map as M"
    let importFn = (\day -> "import qualified Problems" ++ year ++ ".Problem" ++ day ++ " as P" ++ day)  
    let imports = map (importFn . show) [1..25]
    mapM_ (hPutStrLn handle) imports
    hPutStrLn handle ""
    hPutStrLn handle "problems :: (M.Map String (FilePath -> IO String), M.Map String (FilePath -> IO String))"
    hPutStrLn handle "problems = (problemsEasy, problemsHard)"
    hPutStrLn handle ""
    hPutStrLn handle "problemsEasy :: M.Map String (FilePath -> IO String)"
    let itemFn = (\day -> "(\"" ++ day ++ "\", P" ++ day ++ ".runEasy)")
    let listItems = map (itemFn . show) [1..25]
    hPutStr handle "problemsEasy = M.fromList ["
    hPutStr handle (head listItems)
    mapM_ (\item -> hPutStr handle (", " ++ item)) (tail listItems)
    hPutStr handle "]\n"
    hPutStrLn handle ""
    hPutStrLn handle "problemsHard:: M.Map String (FilePath -> IO String)"
    let hardItemFn = (\day -> "(\"" ++ day ++ "\", P" ++ day ++ ".runHard)")
    let listHardItems = map (hardItemFn . show) [1..25]
    hPutStr handle "problemsHard = M.fromList ["
    hPutStr handle (head listHardItems)
    mapM_ (\item -> hPutStr handle (", " ++ item)) (tail listHardItems)
    hPutStr handle "]\n"
    hClose handle
