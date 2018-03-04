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
    let matches = "xxxxxxxx"
    let forcedMatches = forcedValid forcedPairs forbidPairs tooNearPairs matches
    print forcedMatches

    -- Soft Constraints
    let finalMatches = iterateMatches forcedMatches grid tooNearPen
        quality = getQual finalMatches grid tooNearPen

    -- Solution filler
    let solution = makeSolution finalMatches quality
    print solution

    -- Print output file
    -- (Solution will be final string to print out)
    output args solution

-- Go make forced pairs only if they are valid, ie they do not conflict with each other
forcedValid :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [Char] -> [Char]
forcedValid forcedPairs forbidPairs tooNearPairs matches
    | forcedDoubles forcedPairs     = makeForced forcedPairs forbidPairs tooNearPairs matches
    | otherwise                     = "No solution possible!"

-- Make solution string for output
makeSolution :: String -> Int -> String
makeSolution matches quality = "Solution: " ++ matchWithSpaces ++ "; Quality: " ++ qualityString
    where matchWithSpaces = intersperse ' ' matches
          qualityString = show quality
