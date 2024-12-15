module Problems.Y2024 (problems) where

import qualified Data.Map as M
import qualified Problems.Y2024.Problem1 as P1
import qualified Problems.Y2024.Problem2 as P2
import qualified Problems.Y2024.Problem3 as P3
import qualified Problems.Y2024.Problem4 as P4
import qualified Problems.Y2024.Problem5 as P5
import qualified Problems.Y2024.Problem6 as P6
import qualified Problems.Y2024.Problem7 as P7
import qualified Problems.Y2024.Problem8 as P8
import qualified Problems.Y2024.Problem9 as P9
import qualified Problems.Y2024.Problem10 as P10
import qualified Problems.Y2024.Problem11 as P11
import qualified Problems.Y2024.Problem12 as P12
import qualified Problems.Y2024.Problem13 as P13
import qualified Problems.Y2024.Problem14 as P14
import qualified Problems.Y2024.Problem15 as P15
import qualified Problems.Y2024.Problem16 as P16
import qualified Problems.Y2024.Problem17 as P17
import qualified Problems.Y2024.Problem18 as P18
import qualified Problems.Y2024.Problem19 as P19
import qualified Problems.Y2024.Problem20 as P20
import qualified Problems.Y2024.Problem21 as P21
import qualified Problems.Y2024.Problem22 as P22
import qualified Problems.Y2024.Problem23 as P23
import qualified Problems.Y2024.Problem24 as P24
import qualified Problems.Y2024.Problem25 as P25

problems :: (M.Map String (FilePath -> IO String), M.Map String (FilePath -> IO String))
problems = (problemsEasy, problemsHard)

problemsEasy :: M.Map String (FilePath -> IO String)
problemsEasy = M.fromList [("1", P1.runEasy), ("2", P2.runEasy), ("3", P3.runEasy), ("4", P4.runEasy), ("5", P5.runEasy), ("6", P6.runEasy), ("7", P7.runEasy), ("8", P8.runEasy), ("9", P9.runEasy), ("10", P10.runEasy), ("11", P11.runEasy), ("12", P12.runEasy), ("13", P13.runEasy), ("14", P14.runEasy), ("15", P15.runEasy), ("16", P16.runEasy), ("17", P17.runEasy), ("18", P18.runEasy), ("19", P19.runEasy), ("20", P20.runEasy), ("21", P21.runEasy), ("22", P22.runEasy), ("23", P23.runEasy), ("24", P24.runEasy), ("25", P25.runEasy)]

problemsHard:: M.Map String (FilePath -> IO String)
problemsHard = M.fromList [("1", P1.runHard), ("2", P2.runHard), ("3", P3.runHard), ("4", P4.runHard), ("5", P5.runHard), ("6", P6.runHard), ("7", P7.runHard), ("8", P8.runHard), ("9", P9.runHard), ("10", P10.runHard), ("11", P11.runHard), ("12", P12.runHard), ("13", P13.runHard), ("14", P14.runHard), ("15", P15.runHard), ("16", P16.runHard), ("17", P17.runHard), ("18", P18.runHard), ("19", P19.runHard), ("20", P20.runHard), ("21", P21.runHard), ("22", P22.runHard), ("23", P23.runHard), ("24", P24.runHard), ("25", P25.runHard)]
