module Parser where

import SoftConstraints

-- Parsing tuples by passing in lines after given label
getTupleSection :: [String] -> String -> [(Char,Char)]
getTupleSection (x:xs) label
    | x == label    = parseTuple xs
    | otherwise     = getTupleSection xs label

parseTuple :: [String] -> [(Char,Char)]
parseTuple [] = []
parseTuple (x:xs)
    | word == ""    = []
    | otherwise     = pair ++ (parseTuple xs)
    where word = removeSpaces x
          val1 = head $ removeParen x
          val2 = last $ removeParen x
          pair = [(val1,val2)]

-- Parse triples by parsing lines after given label
getTripleSection :: [String] -> String -> [(Char,Char,Int)]
getTripleSection (x:xs) label
    | x == label    = parseTriple xs
    | otherwise     = getTripleSection xs label

parseTriple :: [String] -> [(Char,Char,Int)]
parseTriple [] = []
parseTriple (x:xs)
    | word == ""    = []
    | otherwise     = triple ++ (parseTriple xs)
    where word = removeSpaces x
          values = removeParen x
          val1 = values !! 0
          val2 = values !! 1
          val3 = read [values !! 2]::Int
          triple = [(val1,val2,val3)]

-- Parsing grid by parsing lines after given label
getGridSection :: [String] -> String -> [[Int]]
getGridSection (x:xs) label
    | x == label    = parseGrid xs
    | otherwise     = getGridSection xs label

parseGrid :: [String] -> [[Int]]
parseGrid (x:xs)
    | word == ""    = []
    | otherwise     = [row] ++ (parseGrid xs)
    where word = removeSpaces x
          row = parseRow x

parseRow :: String -> [Int]
parseRow [] = []
parseRow (x:xs)
    | x `elem` numChar      = [aNum] ++ (parseRow xs)
    | x `elem` [' ']        = parseRow xs
    | otherwise             = []
    where aNum = read [x]::Int
          numChar = ['1','2','3','4','5','6','7','8','9','0']

-- Remove extra characters from input lines
removeParen :: String -> String
removeParen word = filter (`notElem` "(,)") word

removeSpaces :: String -> String
removeSpaces word = filter (`notElem` " ") word

-- Check for parsing errors
validMachTask :: [(Char,Char)] -> String
validMachTask [] = ""
validMachTask (x:xs)
    | mach `notElem` machines       = "invalid machine/task"
    | task `notElem` tasks          = "invalid machine/task"
    | otherwise                     = validMachTask xs
    where mach = fst x
          task = snd x

validTaskTask :: [(Char,Char)] -> String
validTaskTask [] = ""
validTaskTask (x:xs)
    | task1 `notElem` tasks         = "invalid machine/task"
    | task2 `notElem` tasks         = "invalid machine/task"
    | otherwise                     = validTaskTask xs
    where task1 = fst x
          task2 = snd x

validTask :: [(Char,Char,Int)] -> String
validTask [] = ""
validTask (x:xs)
    | task1 `notElem` tasks         = "invalid machine/task"
    | task2 `notElem` tasks         = "invalid machine/task"
    | pen < 0                       = "invalid penalty"
    | otherwise                     = validTask xs
    where task1 = triplefst x
          task2 = triplesnd x
          pen = tripletrd x

validGrid :: [[Int]] -> String
validGrid [] = ""
validGrid grid
    | length grid < 8           = "machine penalty error"
    | length grid > 8           = "machine penalty error"
    | otherwise                 = validRow grid

validRow :: [[Int]] -> String
validRow [] = ""
validRow (x:xs)
    | length x < 8          = "machine penalty error"
    | length x > 8          = "machine penalty error"
    | validPen x            = "invalid penalty"
    | otherwise             = validRow xs

validPen :: [Int] -> Bool
validPen [] = False
validPen (x:xs)
    | x < 0         = True
    | otherwise     = validPen xs