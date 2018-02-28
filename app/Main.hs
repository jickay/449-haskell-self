module Main where

import Input
import Parser
import HardConstraints

import System.Environment

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
    print forcedPairs

    let forbidPairs = getTupleSection linesOfFile "forbidden machine:"
    print forbidPairs

    let tooNearPairs = getTupleSection linesOfFile "too-near tasks:"
    print tooNearPairs

    let grid = getGridSection linesOfFile "machine penalties:"
    print grid

    let tooNearPen = getTripleSection linesOfFile "too-near penalities"
    print tooNearPen
    
    -- Hard Constraints
    let matches = ['X','X','X','X','X','X','X','X']
    print (forcedDoubles forcedPairs)
    print (makeForced forcedPairs forbidPairs tooNearPairs matches)

    -- Solution filler
    let solutionOutput = "Solution: A B C D E F G H; Quality: 0"

    -- Print output file
    -- (Solution will be final string to print out)
    output args solutionOutput


