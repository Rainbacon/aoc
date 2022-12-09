module Problem7 where

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Command = Change String | ListSource [FileTree]
        deriving (Show)
data FileTree = Dir String (M.Map String FileTree) | File Int String
        deriving (Show)

runEasy :: FilePath -> IO String
runEasy fp = do
    input <- parseFile parseInput fp
    return (show input)

parseInput :: (Monad m) => ParsecT Void String m FileTree
parseInput = do
              commands <- sepEndBy1 parseCommand eol
              let firstCommand = head commands
              let initial = Dir "/" M.empty
              if isRoot firstCommand then return (fst $ buildTree initial (tail commands)) else fail "Initial command was not move to root"

buildTree :: FileTree -> [Command] -> (FileTree, [Command])
buildTree ft [] = (ft, [])
buildTree ft ((Change ".."):cs) = (ft, cs)
buildTree (Dir n contents) ((Change x):cs) = let downFt = Y.fromJust $ M.lookup x contents
                                                 (nextFt, left) = buildTree downFt cs
                                                 newContents = M.insert x nextFt contents
                                             in case left of
                                                  [] -> ((Dir n newContents), [])
                                                  xs -> buildTree (Dir n newContents) xs
buildTree ft@(Dir name contents) ((ListSource xs):cs) = let newContents = foldl insContent contents xs
                                                        in buildTree ft cs

insContent :: M.Map String FileTree -> FileTree -> M.Map String FileTree
insContent contents d@(Dir name nested) = M.insert name d contents
insContent contents f@(File size name) = M.insert name f contents

isRoot :: Command -> Bool
isRoot (Change "/") = True
isRoot _ = False


parseFtFile :: (Monad m) => ParsecT Void String m FileTree
parseFtFile = do
                size <- read <$> some digitChar
                spaceChar
                name <- some (letterChar <|> char '.')
                eol
                return (File size name)

parseDir :: (Monad m) => ParsecT Void String m FileTree
parseDir = do
            string "dir "
            name <- some letterChar
            eol
            return (Dir name M.empty)

parseCommand :: (Monad m) => ParsecT Void String m Command
parseCommand = do
                string "$ "
                com <- parseChange <|> parseList
                return com

parseChange :: (Monad m) => ParsecT Void String m Command
parseChange = do
               string "cd "
               name <- string "/" <|> some letterChar <|> string ".."
               return (Change name) 

parseList :: (Monad m) => ParsecT Void String m Command
parseList = do
             string "ls"
             eol
             contents <- manyTill (parseDir <|> parseFtFile) (notFollowedBy (parseDir <|> parseFtFile))
             return (ListSource contents)
