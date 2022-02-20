import Data.Time
import Data.Maybe (fromJust)

{-
todo list db for hw assgs
- title, date and time
- via telnet
- add / delete / edit items
- show items due in the next five days
- sub todos?
-}

data Record = Record {
    id :: Int, 
    name :: String,
    dueDay :: Maybe Day,          -- Not required
    dueTime :: Maybe TimeOfDay,   -- Not required
    complete :: Bool,
    children :: [Record]    -- May be empty
}   deriving Show

main :: IO ()
main = do
    putStrLn "TODO App v1.0"
    contents <- readFile "db.txt"
    -- Parse database structure into memory
    let tokens = makeList ',' contents
    let (db, _) = readDB [] tokens
    print tokens
    print db
    inputLoop db


curryParseDay :: String -> Maybe Day
curryParseDay = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-H:%-M"
curryParseTime :: String -> Maybe TimeOfDay
curryParseTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-H:%-M"
parseBool :: String -> Bool
parseBool b
    | b == "True" = True
    | b == "False" = False
    | otherwise = error "Invalid Syntax"


-- Input: List of records so far, list of tokens
-- Output: new list with next record prepended, rest of the tokens afterwards
readDB :: [Record] -> [String] -> ([Record], [String])
readDB records (id:name:datetime:completed:"[":x:xs) = let
    (innerRecords, innerRest) = readDB [] (x:xs)
    (outerRecords, outerRest) = readDB records xs
    in
    (Record 
        (read id :: Int)
        name 
        (curryParseDay datetime :: Maybe Day) 
        (curryParseTime datetime :: Maybe TimeOfDay) 
        (parseBool completed) 
        (if x == "]" then
            []
        else 
            innerRecords)
        : outerRecords,
        if x == "]" then
            outerRest
        else
            innerRest
    )
readDB records ("]":xs) = (records, xs)
readDB _ xs = ([], xs)

-- add <record info>        adds a record, returns the data of the new record
-- get <id>                 returns the data of the given record
-- del <id>                 deletes a record
-- edit <id> <record info>  edits a record
-- quit                     exits the program, commits Db
-- cancel                   resets db cache from file
inputLoop :: [Record] -> IO()
inputLoop db = do
    input <- getLine
    let tokens = makeList ' ' input
    -- call input loop with new structure, created by calling function determined by command
    inputLoop db


-- takes in file contents, breaks into list of tokens
-- Input: delimit character, string to tokenize
-- Output: String broken up by the delimit character
makeList :: Char -> String -> [String]
makeList _ "" = []
makeList delim content = let
    (token, rest) = next delim False content
    in
        token : makeList delim rest
        

-- Input: delimit char, is next char is escaped, input string
-- Output: next word from the given string, and rest of the string after the word
next :: Char -> Bool -> String -> (String, String)
next delim False ('\\':xs) = next delim True xs
next _ False ('[':xs) = ("[", nextNonSpace xs)
next _ False (']':xs) = ("]", nextNonSpace xs)
next delim False ('\n':xs) = next delim False xs
next _ _ "" = ("", "")
next delim _ (x:xs)
    | x == delim = ("", nextNonSpace xs)
    | otherwise = let
        (token, rest) = next delim False xs
        in
            (x : token, rest)


-- Input: Takes in a string that starts with a few spaces
-- Output: Returns the substring that starts where the spaces end
nextNonSpace :: String -> String
nextNonSpace (' ':xs) = nextNonSpace xs
nextNonSpace s = s