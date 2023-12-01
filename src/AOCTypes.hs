module AOCTypes where

import qualified Data.Map as M

type ProblemSet = M.Map String (FilePath -> IO String) 