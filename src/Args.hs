module Args (
    opts 
  , ProblemPart (..)
  , AocOptions (..)
) where

import Options.Applicative

data InputFile = Test | Input

instance Show InputFile where
  show Test = "test.txt"
  show Input = "input.txt"

data ProblemPart = EasyProblem | HardProblem

data AocOptions = AocOptions {
    optYear :: String
  , optDay :: String
  , optMode :: InputFile
  , optPart :: ProblemPart
}

opts :: ParserInfo AocOptions
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Run an Advent of Code problem for DAY and YEAR"
  <> header "aoc - a runner for Advent of Code solutions")

options :: Parser AocOptions
options = AocOptions <$> year <*> day <*> mode <*> part

year :: Parser String
year = strOption
  (  long "year" 
  <> short 'y' 
  <> metavar "YEAR"
  <> help "Which Advent of Code year should be run")

day :: Parser String
day = strOption
  (  long "day"
  <> short 'd'
  <> metavar "DAY"
  <> help "Which day of Advent of code should be run")

mode :: Parser InputFile
mode = testMode <|> inputMode

testMode :: Parser InputFile
testMode = flag' Test
  (  long "test"
  <> short 't'
  <> help "Run the test input")

inputMode :: Parser InputFile
inputMode = flag' Input
  (  long "input"
  <> short 'i'
  <> help "Run the actual input")

part :: Parser ProblemPart
part = easyPart <|> hardPart

easyPart :: Parser ProblemPart
easyPart = flag' EasyProblem
  (  long "easy"
  <> short 'e'
  <> help "Run the easy problem")

hardPart :: Parser ProblemPart
hardPart = flag' HardProblem
  (  long "hard"
  <> short 'h'
  <> help "Run the hard problem")