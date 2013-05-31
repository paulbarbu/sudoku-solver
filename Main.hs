import System.Environment(getArgs)

import SudokuSolver

main = do
    [fileName] <- getArgs -- input validation
    contents <- readFile fileName -- read one line -- input validation
    putStrLn $ concatMap show $ solve $ lineToBoard contents
