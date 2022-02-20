import System.IO

{-
todo list db for hw assgs
- title, date and time
- via telnet
- add / delete / edit items
- show items due in the next five days
Record:
id, title,         year, m, d,  h,  m
"CSC 372 hw 5" 2022 3 1 23 59
-}

data Record = Record {
    id :: Int, 
    name :: String,
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    min :: Int
}   deriving Show

main :: IO ()
main = do
    putStrLn "TODO App v1.0"
    contents <- readFile "db.txt"
    print $ makeList contents
    -- create DB database structure in memory
    inputLoop

-- add <record info>        adds a record, returns the data of the new record
-- get <id>                 returns the data of the given record
-- del <id>                 deletes a record
-- edit <id> <record info>  edits a record
-- quit                     exits the program, commits Db
-- cancel                   resets db cache from file
inputLoop :: IO()
inputLoop = do
    input <- getLine
    let tokens = makeList input
    -- call input loop with new structure, created by calling function determined by command
    inputLoop


-- takes in file contents, breaks into list of tokens
makeList :: [Char] -> [String]
makeList "" = []
makeList content = let
    (token, rest) = next content
    in
        token : makeList rest
        

-- returns the next word from the given string, along with the rest of the
-- string after the word
next :: String -> (String, String)
next "" = ("", "")
next (' ':xs) = ("", xs)
next ('\n':xs) = ("\n", xs)
next ('"':xs) = nextString xs
next (x:xs) = let
    (token, rest) = next xs
    in
        (x : token, rest)

nextString :: String -> (String, String)
nextString [] = error "Syntax error"
nextString ('"':xs) = ("", nextNonSpace xs)
    where
        nextNonSpace (' ':xs) = nextNonSpace xs
        nextNonSpace s = s
nextString (x:xs) = let
    (token, rest) = nextString xs
    in
        (x:token, rest)

