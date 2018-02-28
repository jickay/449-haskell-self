module HardConstraints where

-- Check if same machine/task forced more than once
forcedDoubles :: [(Char,Char)] -> Bool
forcedDoubles [] = True
forcedDoubles (x:xs)
    | mach `elem` mach'     = False
    | task `elem` task'     = False
    | otherwise             = forcedDoubles xs
    where mach = fst x
          task = snd x
          mach' = getMachs xs
          task' = getTasks xs

getMachs :: [(Char,Char)] -> [Char]
getMachs [] = []
getMachs (x:xs) =
    let mach = fst x
    in [mach] ++ getMachs xs

getTasks :: [(Char,Char)] -> [Char]
getTasks [] = []
getTasks (x:xs) =
    let task = fst x
    in [task] ++ getTasks xs

-- -- Check if forced pair is also forbidden
-- forcedForbid :: [(Char,Char)] -> [(Char,Char)] -> Bool
-- forcedForbid [] _ = True
-- forcedForbid _ [] = True
-- forcedForbid (x:xs) ys
--     | x `elem` ys       = False
--     | otherwise         = forcedForbid xs ys

-- Make forced pairs
makeForced :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [Char] -> [Char]
makeForced [] _ _ matches = matches
makeForced (x:xs) forbidden tooNear matches
    | checkForbid x forbidden       = "No possible solution"
    | otherwise                     = makePair x (makeForced xs forbidden tooNear matches) tooNear forbidden

-- Check if pair to match is forbidden; returns True if forbidden
checkForbid :: (Char,Char) -> [(Char,Char)] -> Bool
checkForbid x forbidden
    | x `elem` forbidden    = True
    | otherwise             = False

-- Make a matched pair
makePair :: (Char,Char) -> [Char] -> [(Char,Char)] -> [(Char,Char)] -> [(Char,(Char,Char))]
makePair x matches tooNear forbidden = do
    let mach = fst x     -- turn num string into int index, eg. "1" -> 0
        task = snd x
        index = (read [mach]::Int) - 1
        (as,bs) = splitAt index matches
    forbidden <- forbidden ++ [(checkTooNear tooNear task mach)]
    newMatches <- as ++ [task] ++ (tail bs)
    return (newMatches, forbidden)

-- Check too-near for new forbidden pairs
checkTooNear :: [(Char,Char)] -> Char -> Char -> (Char,Char)
checkTooNear (x:xs) task mach
    | task == taskL         = (machR,taskL)
    | task == taskR         = (machL,taskR)
    | otherwise             = checkTooNear xs task mach
    where taskL = fst x
          taskR = snd x
          machL = getMachL mach
          machR = getMachR mach

-- Get neighbour machine for too-near checks
getMachL :: Char -> Char
getMachL mach
    | mach == '1'       = '8'
    | otherwise         = head newMach
    where machInt = (read [mach]::Int) - 1
          newMach = show machInt

getMachR :: Char -> Char
getMachR mach
    | mach == '8'       = '1'
    | otherwise         = head newMach
    where machInt = (read [mach]::Int) + 1
          newMach = show machInt