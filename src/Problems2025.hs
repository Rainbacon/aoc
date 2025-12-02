module Problems2025 (problems) where

import qualified Data.Map as M
import qualified Problems2025.Problem1 as P1
import qualified Problems2025.Problem2 as P2
import qualified Problems2025.Problem3 as P3
import qualified Problems2025.Problem4 as P4
import qualified Problems2025.Problem5 as P5
import qualified Problems2025.Problem6 as P6
import qualified Problems2025.Problem7 as P7
import qualified Problems2025.Problem8 as P8
import qualified Problems2025.Problem9 as P9
import qualified Problems2025.Problem10 as P10
import qualified Problems2025.Problem11 as P11
import qualified Problems2025.Problem12 as P12

problems :: (M.Map String (FilePath -> IO String), M.Map String (FilePath -> IO String))
problems = (problemsEasy, problemsHard)

problemsEasy :: M.Map String (FilePath -> IO String)
problemsEasy = M.fromList [("1", P1.runEasy), ("2", P2.runEasy), ("3", P3.runEasy), ("4", P4.runEasy), ("5", P5.runEasy), ("6", P6.runEasy), ("7", P7.runEasy), ("8", P8.runEasy), ("9", P9.runEasy), ("10", P10.runEasy), ("11", P11.runEasy), ("12", P12.runEasy)]

problemsHard:: M.Map String (FilePath -> IO String)
problemsHard = M.fromList [("1", P1.runHard), ("2", P2.runHard), ("3", P3.runHard), ("4", P4.runHard), ("5", P5.runHard), ("6", P6.runHard), ("7", P7.runHard), ("8", P8.runHard), ("9", P9.runHard), ("10", P10.runHard), ("11", P11.runHard), ("12", P12.runHard)]
