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

-- Check if forced pair is also forbidden
forcedForbid :: [(Char,Char)] -> [(Char,Char)] -> Bool
forcedForbid [] _ = True
forcedForbid _ [] = True
forcedForbid (x:xs) ys
    | x `elem` ys       = False
    | otherwise         = forcedForbid xs ys

-- Make forced pairs
makeForced :: [(Char,Char)] -> [Char]
makeForced [] = []
makeForced (x:xs) =
    let mach = read [fst x]::Int
        task = snd x
        matches = ['X','X','X','X','X','X','X','X']
        (as,bs) = splitAt mach matches
        b = tail bs
    in as ++ [task] ++ b