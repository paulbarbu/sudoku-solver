import System.Environment(getArgs)

import SudokuSolver

main = do
    [fileName] <- getArgs -- input validation
    contents <- readFile fileName -- input validation
    print $ solve 0 $ lineToBoard $ init contents
