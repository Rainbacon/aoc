module Problem7 where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Raw = C String | L | D String | F Integer String
    deriving (Show)
data Command = Change String | ListSource [FileObject]
        deriving (Show)
data FileObject = Dir String | File Integer String
        deriving (Show)
type FileSystem = M.Map String [FileObject]

toStr :: Int -> String -> FileSystem -> [String]
toStr indent path fs = line:(concat (map (toStr' indent fs) (Y.fromJust $ M.lookup path fs)))
                    where ind = replicate indent ' '
                          line = ind ++ "- " ++ path

toStr' indent fs (Dir s) = toStr (indent + 2) s fs
toStr' indent fs (File i n) = [(replicate indent ' ') ++ "  - " ++ show i ++ " " ++ n]


runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return $ show $ sum $ filter (<= 100000) $ map (sizeDir input) $ M.keys input

sizeDir :: FileSystem -> String -> Integer
sizeDir fs name = sum $ map size (Y.fromJust $ M.lookup name fs)
              where size (File s _) = s
                    size (Dir n) = sizeDir fs n 

parseInput :: (Monad m) => ParsecT Void String m FileSystem
parseInput = do
              rawCommands <- sepEndBy1 parseCommand eol
              let commands = buildCommands rawCommands
              return $ runCommands [] commands M.empty 

runCommands :: [String] -> [Command] -> FileSystem -> FileSystem
runCommands _ [] fs = fs
runCommands dirs ((Change "/"):cs) fs = runCommands ["/"] cs fs
runCommands (dir:ds) ((Change ".."):cs) fs = runCommands ds cs fs
runCommands dirs ((Change name):cs) fs = runCommands (name:dirs) cs fs
runCommands dirs@(dir:ds) ((ListSource contents):cs) fs = let newFs = M.insert (concat dirs) contents fs
                                                       in runCommands dirs cs newFs 
runCommands _ _ fs = error "how did I get here"

buildCommands :: [Raw] -> [Command]
buildCommands [] = []
buildCommands ((C name):cs) = (Change name):(buildCommands cs)
buildCommands ((L):cs) = let contents = takeWhile isContent cs
                             rest = dropWhile isContent cs
                         in (ListSource (map toFt contents)):(buildCommands rest)

toFt :: Raw -> FileObject
toFt (D name) = Dir name
toFt (F size name) = File size name
toFt _ = error "unable to convert"

isContent :: Raw -> Bool
isContent (D _) = True
isContent (F _ _) = True
isContent _ = False

parseFtFile :: (Monad m) => ParsecT Void String m Raw
parseFtFile = do
                size <- read <$> some digitChar
                spaceChar
                name <- some (letterChar <|> char '.')
                return (F size name)

parseDir :: (Monad m) => ParsecT Void String m Raw
parseDir = do
            string "dir "
            name <- some letterChar
            return (D name)

parseCommand :: (Monad m) => ParsecT Void String m Raw
parseCommand = parseChange <|> parseList <|> parseDir <|> parseFtFile

parseChange :: (Monad m) => ParsecT Void String m Raw
parseChange = do
               string "$ cd "
               name <- string "/" <|> some letterChar <|> string ".."
               return (C name) 

parseList :: (Monad m) => ParsecT Void String m Raw
parseList = do
             string "$ ls"
             return L 
