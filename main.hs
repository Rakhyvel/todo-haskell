import Data.Time
import Data.Maybe


data Database = Database {
    filename :: String,
    records :: [Record]
} deriving Show

data Record = Record {
    id :: Int, 
    name :: String,
    dueDay :: Maybe Day,
    dueTime :: Maybe TimeOfDay,
    complete :: Bool
}   deriving Show


main :: IO ()
main = do
    putStrLn "TODO App v1.0"
    putStrLn "By Joseph Shimel"
    db <- readDB "db.txt"
    inputLoop db


-- Parses UTC date/time string to Day type
curryParseDay :: String -> Maybe Day
curryParseDay = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-H:%-M:%-S"
-- Parses UTC date/time string to TimeOfDay type
curryParseTime :: String -> Maybe TimeOfDay
curryParseTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-H:%-M:%-S"
-- Parses string to Bool
parseBool :: String -> Bool
parseBool b
    | b == "True" = True
    | b == "False" = False
    | otherwise = error "invalid Syntax"


createRecord :: Int -> String -> String -> String -> Record
createRecord id name datetime completed = 
    Record
    id
    name
    (curryParseDay datetime :: Maybe Day) 
    (curryParseTime datetime :: Maybe TimeOfDay) 
    (parseBool completed)


-- Input: filename where database file is
-- Output: List of records (represents DB)
readDB :: String -> IO Database
readDB filename = do
    contents <- readFile filename
    let tokens = makeList ',' contents
    let (db, _) = readRecords [] tokens
    return $ Database filename db


-- Input: List of records so far, list of tokens
-- Output: new list with next record prepended, rest of the tokens afterwards
readRecords :: [Record] -> [String] -> ([Record], [String])
readRecords records (id:name:datetime:completed:xs) = let
    (outerRecords, outerRest) = readRecords records xs
    in
    (createRecord (read id) name datetime completed
        : outerRecords,
        outerRest
    )
readRecords _ xs = ([], xs)


reprRecords :: [Record] -> String
reprRecords [] = ""
reprRecords (x:xs) = let
    day = Main.dueDay x
    time = Main.dueTime x
    in
    show (Main.id x) ++ ", " ++
    Main.name x ++ ", " ++
    (if isJust day then show (fromJust $ Main.dueDay x) else "") ++ 
    (if isJust day && isJust time then "T" else "") ++
    (if isJust time then show (fromJust $ Main.dueTime x) else "") ++ ", " ++
    show (Main.complete x) ++ "\n" ++ 
    reprRecords xs


inputLoop :: Database -> IO()
inputLoop db = do
    print db
    input <- getLine
    let tokens = makeList ',' input
    if head tokens == "quit" then
        putStrLn "Goodbye!"
    else if head tokens == "commit" then do
        writeFile (filename db) (reprRecords $ records db)
        inputLoop db
    else do
        newDB <- switch tokens db
        inputLoop newDB


switch :: [String] -> Database -> IO Database
switch (x:xs) db = case x of
    "add" -> return $ Database (filename db) (add xs (records db))
    "edit" -> return $ Database (filename db) (edit xs (records db))
    "del" -> return $ Database (filename db) (delete xs (records db))
    "reset" -> readDB "db.txt"
    _ -> error ("not a command " ++ x)
switch [] _ = error "no command given"


maxID :: Int -> [Record] -> Int
maxID id [] = id
maxID id (x:xs) =
    if Main.id x > id then
        maxID (Main.id x) xs
    else
        maxID id xs


-- Input: list of tokens from command, db
-- Output: db after adding a record to it
add :: [String] -> [Record] -> [Record]
add [name, datetime, completed] db = 
    createRecord (maxID 0 db + 1) name datetime completed
    : db
add [] _ = error "no commands"
add _ _ = error "not correct format"


-- Input: id of record to edit and record info, database
-- Output: new database with record info edited
edit :: [String] -> [Record] -> [Record]
edit [id, name, datetime, completed] = map (\x -> 
    if Main.id x == read id then 
        createRecord (read id) name datetime completed 
    else
        x)
edit _ = error "invalid syntax"


-- Input: id of record to delete, database
-- Output: new database with record deleted
delete :: [String] -> [Record] -> [Record]
delete [id] = filter (\x -> Main.id x /= read id)
delete _ = error "expected id"


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
next delim False ('\n':xs) = ("", nextNonSpace xs)
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