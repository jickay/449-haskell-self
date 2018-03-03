module SoftConstraints where

import Data.List

machines :: [Char]
machines = "12345678"

tasks :: [Char]
tasks = "ABCDEFGH"

-- Get quality based on available machines
getQual :: [Char] -> [[Int]] -> [(Char,Char,Int)] -> Int
getQual (x:xs) grid tooNearPen
    | x `elem` tasks    = (xPen + nearPen + getQual xs grid tooNearPen)
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
          expandedMatches = expandMatches matches freeTasks
          rounds = getRounds expandedMatches grid
          index = compareMatches rounds grid tooNearPen
          bestIteration = expandedMatches !! index

expandMatches :: [Char] -> [Char] -> [[Char]]
expandMatches [] _ = []
expandMatches matches freeTasks
    | x == 'x'          = [matches'] ++ expandMatches matches (tail freeTasks)
    | otherwise         = expandMatches matches freeTasks
    where (x:_) = matches
          index = getIndex (elemIndex x matches)
          (as,bs) = splitAt index matches
          matches' = as ++ [(head freeTasks)] ++ (tail bs)

getRounds :: [[Char]] -> [[Int]] -> [[Char]]
getRounds [] _ = []
getRounds expandedMatches grid
    | x /= []           = [fillRound x grid] ++ getRounds xs grid
    | otherwise         = getRounds xs grid
    where (x:xs) = expandedMatches

fillRound :: [Char] -> [[Int]] -> [Char]
fillRound [] _ = []
fillRound matches grid
    | x == 'x'          = [fillX i row freeTasks] ++ fillRound xs grid
    | otherwise         = [x] ++ fillRound xs grid
    where (x:xs) = matches
          freeTasks = getFreeTasks tasks matches
          mach = getIndex (elemIndex x xs)
          row = grid !! mach
          i = minIndex row

fillX :: Int -> [Int] -> [Char] -> Char
fillX x [] _ = (tasks !! x)
fillX taskIndex row freeTasks
    | task `elem` freeTasks             = task
    | otherwise                         = fillX (taskIndex+1) row freeTasks
    where task = tasks !! taskIndex

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


