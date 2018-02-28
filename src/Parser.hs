module Parser where

sections :: [String]
sections = ["Name:",
            "forced partial assignment:",
            "forbidden machine:",
            "too-near tasks:",
            "machine penalties:",
            "too-near penalities"]

checkSection :: [String] -> [String] -> String
checkSection (x:xs) ys 
    | x == []       = checkSection xs ys
    | x == y        = y
    where (y:_) = ys

getTupleSection :: [String] -> String -> [(Char,Char)]
getTupleSection (x:xs) label
    | x == label    = parseTuple xs
    | otherwise     = getTupleSection xs label

getTripleSection :: [String] -> String -> [(Char,Char,Int)]
getTripleSection (x:xs) label
    | x == label    = parseTriple xs
    | otherwise     = getTripleSection xs label

getGridSection :: [String] -> String -> [[Int]]
getGridSection (x:xs) label
    | x == label    = parseGrid xs
    | otherwise     = getGridSection xs label

parseTuple :: [String] -> [(Char,Char)]
parseTuple [] = []
parseTuple (x:xs)
    | word == ""    = []
    | otherwise     = pair ++ (parseTuple xs)
    where word = removeSpaces x
          val1 = head $ removeParen x
          val2 = last $ removeParen x
          pair = [(val1,val2)]

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

removeParen :: String -> String
removeParen word = filter (`notElem` "(,)") word

removeSpaces :: String -> String
removeSpaces word = filter (`notElem` " ") word