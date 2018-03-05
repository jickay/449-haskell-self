module SoftConstraints where

import Data.List

machines :: [Char]
machines = "12345678"

tasks :: [Char]
tasks = "ABCDEFGH"

-- Get quality based on available machines
getQual :: [Char] -> [[Int]] -> [(Char,Char,Int)] -> Int
getQual [] _ _ = 0
getQual matches grid tooNearPen
    | x `elem` tasks    = xPen + nearPen + getQual xs grid tooNearPen
    | otherwise         = 0 + nearPen + (getQual xs grid tooNearPen)
    where (x:xs) = matches
          machInt = getIndex (elemIndex x matches)
          taskInt = getIndex (elemIndex x tasks)
          xPen = (grid !! machInt) !! taskInt
          nearPen = getNearPen machInt x tooNearPen matches

getNearPen :: Int -> Char -> [(Char,Char,Int)] -> [Char] -> Int
getNearPen _ _ _ [] = 0
getNearPen _ _ [] _ = 0
getNearPen mach task tooNearPen matches
    | task == taskL         = checkNeighbor matches machR taskR pen + getNearPen mach task xs matches
    | task == taskR         = checkNeighbor matches machL taskL pen + getNearPen mach task xs matches
    | otherwise             = getNearPen mach task xs matches
    where (x:xs) = tooNearPen
          taskL = triplefst x
          taskR = triplesnd x
          pen = tripletrd x
          machL = getMachL mach
          machR = getMachR mach

checkNeighbor :: [Char] -> Int -> Char -> Int -> Int
checkNeighbor [] _ _ _ = 0
checkNeighbor matches mach task pen
    | (matches !! mach) == task         = pen
    | otherwise                         = 0

triplefst :: (a,b,c) -> a
triplefst (a,_,_) = a

triplesnd :: (a,b,c) -> b
triplesnd (_,b,_) = b

tripletrd :: (a,b,c) -> c
tripletrd (_,_,c) = c

-- Get neighbour machine for too-near checks
getMachL :: Int -> Int
getMachL mach
    | mach <= 0         = 7
    | otherwise         = mach - 1

getMachR :: Int -> Int
getMachR mach
    | mach >= 7         = 0
    | otherwise         = mach + 1


-- Iterate through different possible matches and return best option
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

-- Fill first unmatches row with all possible free tasks
-- Creates a set of potential matches to compare
expandMatches :: Int -> [Char] -> [Char] -> [[Char]]
expandMatches _ [] _ = []
expandMatches _ _ [] = []
expandMatches index matches freeTasks
    | matches !! index == 'x'       = [matches'] ++ expandMatches 0 matches (tail freeTasks)
    | otherwise                     = expandMatches (index+1) matches freeTasks
    where (as,bs) = splitAt index matches
          matches' = as ++ [(head freeTasks)] ++ (tail bs)

-- Methods to fill out expanded matches
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

-- Compare a list of matches to return the one with lowest penalties
compareMatches :: [[Char]]-> [[Int]] -> [(Char,Char,Int)] -> Int
compareMatches [] _ _ = 0
compareMatches listOfMatches grid tooNearPen =
    let bestMatch = foldl1 (\acc x -> if (getQual x grid tooNearPen) > (getQual acc grid tooNearPen) then acc else x) listOfMatches
        index = getIndex (elemIndex bestMatch listOfMatches)
    in index

-- Return all unmatched tasks that are free to be assigned
getFreeTasks :: [Char] -> [Char] -> [Char]
getFreeTasks [] _ = []
getFreeTasks _ [] = []
getFreeTasks (x:xs) matches
    | x `notElem` matches       = [x] ++ getFreeTasks xs matches
    | otherwise                 = getFreeTasks xs matches

-- Return index of an element in a list
getIndex :: Maybe Int -> Int
getIndex mx = case mx of
    Just x -> x
    Nothing -> 0
