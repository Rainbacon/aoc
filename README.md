# aoc
This repository contains multiple years of Advent of Code (AOC) challenge solutions in addition to a command line interface to run them.

## Running the application
The project is configured using [stack](https://docs.haskellstack.org/en/stable/). To compile the application

```
stack build
```

To run the application, you can use `stack run` and pass in the following options

- `-y|--year YEAR` Which competition year to run
- `-d|--day DAY` Which competition day to run
- `-t|--test` Run the test input from the [default file location](#input-files)
- `-i|--input` Run the user's unique puzzle input from the default file location
- `-e|--easy` Run part 1 (the "easy" part)
- `-h|--hard` Run part 2 (the "hard' part)
- `-c|--custom-file CUSTOMFILE` Run an input from a custom file location

## <a name="input-files"></a> Input Files
The solutions in this repository are designed to operate on input files. The runner enforces a folder hierarchy for input files. To provide input for a specific day
the file should live in the `data/<YEAR>/<DAY>` directory. By default, the runner will look for a file called `test.txt` for the test input or `input.txt` for the 
user's unique input. You can use a file of a different name by specifying it with `-c CUSTOMFILE` but the file still needs to be in that directory. 

## Project Structure

Solutions are grouped into modules based on the year of the challenge. Each of these modules exposes the following type `problems :: (Map String (FilePath -> IO String), Map String (FilePath -> IO String))`. The first element of the tuple is a mapping between the day of the challenge and the part 1 solution. The second element
is a mapping between the day of the challenge and the part 2 solution. Each solution exposes two functions `runEasy :: FilePath -> IO String` and `runHard :: FilePath -> IO String` which get assembled into the map returned out of the year's module. The runner passes a file handle into each solution function which is then responsible for reading the input from the file as it sees fit. Most solutions will operate either on the raw string values contained in the file or parse them using [Megaparsec](https://hackage.haskell.org/package/megaparsec). For this reason, the `Utils.Parsing` package provides two helper functions `parseFile` which runs the file text through a supplied parser and `parseFast` which applies a `String -> a` transformation function to the text of the file. 