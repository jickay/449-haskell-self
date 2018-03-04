module SoftConstraints where

import Data.List

machines :: [Char]
machines = "12345678"

tasks :: [Char]
tasks = "ABCDEFGH"

-- Get quality based on available machines
getQual :: [Char] -> [[Int]] -> [(Char,Char,Int)] -> Int
getQual [] _ _ = 0
getQual (x:xs) grid tooNearPen
    | x `elem` tasks    = xPen + nearPen + getQual xs grid tooNearPen
    | otherwise         = 0 + nearPen + (getQual xs grid tooNearPen)
    where mach = getIndex (elemIndex x xs)
          task = getIndex (elemIndex x tasks)
          xPen = (grid !! mach) !! task
          nearPen = getNearPen (head (show mach)) x tooNearPen

getNearPen :: Char -> Char -> [(Char,Char,Int)] -> Int
getNearPen a b c = 0

getIndex :: Maybe Int -> Int
getIndex mx = case mx of
    Just x -> x
    Nothing -> 0

iterateMatches :: [Char] -> [[Int]] -> [(Char,Char,Int)] ->[Char]
iterateMatches [] _ _ = []
iterateMatches matches grid tooNearPen
    | freeTasks == []       = matches
    | otherwise             = iterateMatches bestIteration grid tooNearPen
    where freeTasks = getFreeTasks tasks matches
          expandedMatches = expandMatches 0 matches freeTasks
          rounds = getRounds expandedMatches grid
          index = compareMatches rounds grid tooNearPen
          bestIteration = rounds !! index

expandMatches :: Int -> [Char] -> [Char] -> [[Char]]
expandMatches _ [] _ = []
expandMatches _ _ [] = []
expandMatches index matches freeTasks
    | matches !! index == 'x'       = [matches'] ++ expandMatches 0 matches (tail freeTasks)
    | otherwise                     = expandMatches (index+1) matches freeTasks
    where (as,bs) = splitAt index matches
          matches' = as ++ [(head freeTasks)] ++ (tail bs)

getRounds :: [[Char]] -> [[Int]] -> [[Char]]
getRounds [] _ = []
getRounds expandedMatches grid
    | x /= []           = [fillRound x grid freeTasks] ++ getRounds xs grid
    | otherwise         = getRounds xs grid
    where (x:xs) = expandedMatches
          freeTasks = getFreeTasks tasks x

fillRound :: [Char] -> [[Int]] -> [Char] -> [Char]
fillRound [] _ _ = []
fillRound matches grid freeTasks
    | x == 'x'          = [fill] ++ fillRound xs grid freeTasks'
    | otherwise         = [x] ++ fillRound xs grid freeTasks
    where (x:xs) = matches
          mach = getIndex (elemIndex x xs)
          row = grid !! mach
          fill = fillX 0 row freeTasks
          freeTasks' = delete fill freeTasks

fillX :: Int -> [Int] -> [Char] -> Char
fillX x [] _ = (tasks !! x)
fillX taskIndex row freeTasks
    | isMin && isFree          = task
    | otherwise                = fillX (taskIndex+1) row freeTasks
    where task = tasks !! taskIndex
          minVal = minimum row
          isMin = (row !! taskIndex) == minVal
          isFree = task `elem` freeTasks

-- validFill :: Char -> Int -> [Int] -> Bool
-- validFill task taskIndex row
--     | isMin && isFree       = True
--     | otherwise             = False
--     where minVal = minimum row
--           isMin = (row !! taskIndex) == minVal
--           isFree = task `elem` freeTasks

minIndex :: [Int] -> Int
minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]
          

getFreeTasks :: [Char] -> [Char] -> [Char]
getFreeTasks [] _ = []
getFreeTasks _ [] = []
getFreeTasks (x:xs) matches
    | x `notElem` matches       = [x] ++ getFreeTasks xs matches
    | otherwise                 = getFreeTasks xs matches

compareMatches :: [[Char]]-> [[Int]] -> [(Char,Char,Int)] -> Int
compareMatches [] _ _ = 0
compareMatches listOfMatches grid tooNearPen =
    let bestMatch = foldl1 (\acc x -> if (getQual x grid tooNearPen) > (getQual acc grid tooNearPen) then acc else x) listOfMatches
        index = getIndex (elemIndex bestMatch listOfMatches)
    in index


