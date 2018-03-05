module Main where

    import Input
    import Parser
    import HardConstraints
    import SoftConstraints
    
    import System.Environment
    import Data.List
    
    main :: IO()
    main = do
        -- Store arguments in arg
        args <- getArgs
        -- Get input
        contents <- input args
        let linesOfFile = lines contents
        print linesOfFile
    
        -- Parser
        let forcedPairs = getTupleSection linesOfFile "forced partial assignment:"
            forbidPairs = getTupleSection linesOfFile "forbidden machine:"
            tooNearPairs = getTupleSection linesOfFile "too-near tasks:"
            grid = getGridSection linesOfFile "machine penalties:"
            tooNearPen = getTripleSection linesOfFile "too-near penalities"
        print forcedPairs
        print forbidPairs
        print tooNearPairs
        print grid
        print tooNearPen

        outputError args (validMachTask forcedPairs)
        outputError args (validMachTask forbidPairs)
        outputError args (validTaskTask tooNearPairs)
        outputError args (validGrid grid)
        outputError args (validTask tooNearPen)
        outputError args (forcedDoubles forcedPairs)

        -- Hard Constraints
        let matches = "xxxxxxxx"
            forcedMatches = makeForced forcedPairs forbidPairs tooNearPairs matches
        outputError args (forcedMatchValid forcedMatches)
        print ("Forced matches: " ++ forcedMatches)
    
        -- Soft Constraints
        let finalMatches = iterateMatches forcedMatches grid tooNearPen
            quality = getQual finalMatches grid tooNearPen
        outputError args (solutionValid finalMatches)
    
        -- Solution filler
        let solution = makeSolution finalMatches quality
        print solution
    
        -- Print output file
        output args solution
    
    -- Go make forced pairs only if they are valid, ie they do not conflict with each other
    -- forcedValid :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [Char] -> [Char]
    -- forcedValid forcedPairs forbidPairs tooNearPairs matches
    --     | forcedDoubles forcedPairs     = makeForced forcedPairs forbidPairs tooNearPairs matches
    --     | otherwise                     = "partial assignment error"
    
    -- Make solution string for output
    makeSolution :: String -> Int -> String
    makeSolution matches quality = "Solution " ++ matchWithSpaces ++ "; Quality: " ++ qualityString
        where matchWithSpaces = intersperse ' ' matches
              qualityString = show quality

    forcedMatchValid :: String -> String
    forcedMatchValid [] = ""
    forcedMatchValid matches =
        if matches == "No valid solution possible!" then matches else ""

    solutionValid :: String -> String
    solutionValid [] = ""
    solutionValid (x:xs)
        | x == 'x'          = "No valid solution possible!"
        | otherwise         = solutionValid xs