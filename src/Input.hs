module Input where

input :: [String] -> IO String
input args =
    -- Get lines from input file name (arg 1)
    getLines (args !! 0)

output :: [FilePath] -> String -> IO()
output args contents =
    -- Write to output file name (arg 2)
    writeFile (args !! 1) contents

getLines :: FilePath -> IO String
getLines fileName = readFile fileName