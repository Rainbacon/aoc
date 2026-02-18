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
Each solution is implemented in is own module which exposes an interface of two functions

```
runEasy :: FilePath -> IO String
runHard :: FilePath -> IO String
```

The solution functions take a file path as their input and are themselves responsible for reading the input from the file and performing the logic of the solution. This allows each solution to parse the input however it makes sense. Most of the solutions parse the input either by applying a `String -> a` transformation function or by running a parser creted using [Megaparsec](https://hackage.haskell.org/package/megaparsec). The `Utils.Parsing` module provides two helpers `parseFile :: (MonadIO m) => ParsecT Void String m a -> FilePath -> m a` and `parseFast :: (MonadIO m) => (String -> a) -> FilePath -> m a` which handle reading the input from the file and passing the loaded string to the parsing function.

The solutions are bundled into a module for each year of the competition. The solutions for each year of the competition are assembled into a module that exposes a tuple containing a collection of the part 1 solutions and a collection of the part 2 solutions. The collections are implemented as maps where the keys are the day of
the competition and the values are the solution functions.