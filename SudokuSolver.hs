module SudokuSolver(
 solve
, lineToBoard
, squareToBoard
)
where

-- TODO pretty print
-- return a list of lists in solve

import Data.Char(digitToInt)
import Data.List(nubBy, (\\), union)
import System.IO

-- Transforms something like "123" in [1,2,3]
lineToBoard :: String -> [Int]
lineToBoard = map digitToInt

-- Transforms something like "1 2 3\n4 5 6" in [1,2,3,4,5,6]
squareToBoard :: String -> [Int]
squareToBoard contents = map read $ concat . map words $ lines contents

{- Given an element's index on the board (between 0 and 80) return all indexes
 - on the same line in a 9x9 board
 -}
getLineIndexes :: Int -> [Int]
getLineIndexes index = take 9 [first..]
    where first = index - (index `mod` 9)

{- Given an element's index on the board (between 0 and 80) return all indexes
 - on the same column in a 9x9 board
 -
 - If the user wants to retrieve the indexes on the same column with the index
 - 41 then he will get: [5,14..77]
 -}
getColIndexes :: Int -> [Int]
getColIndexes index = take 9 [first,first+9 ..]
    where first = index `mod` 9

{- Given an element's index on the board (between 0 and 80) return all indexes
 - in the same 3x3 box in a 9x9 board
 -
 - If the user wants to retrieve the indexes in the same box with the index
 - 41 then he will get: [30,31,32,39,40,41,48,49,50]
 -}
getBoxIndexes :: Int -> [Int]
getBoxIndexes index = concatMap (\a -> [start+a*9..start+a*9+2]) [0,1,2]
    where start = 27 * (index `div` 27) + 3 * ((index `mod` 9) `div` 3)

getLineElems :: Int -> [Int] -> [Int]
getLineElems index board = map (board!!) $ getLineIndexes index

getColElems :: Int -> [Int] -> [Int]
getColElems index board = map (board!!) $ getColIndexes index

getBoxElems :: Int -> [Int] -> [Int]
getBoxElems index board = map (board!!) $ getBoxIndexes index

availableElems :: Int -> [Int] -> [Int]
availableElems index board = [1..9] \\
    getLineElems index board `union` getColElems index board
    `union` getBoxElems index board

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index newElem xs = left ++ [newElem] ++ drop 1 right
    where (left,right) = splitAt index xs

{- Solve a sudoku board
 -
 - If the current position (n) is 0 then it needs solving which is done by
 - filling it with the first available element then by moving forward to the
 - next element
 -
 - If n is 80 then it means we reached the end of a solved board, so we print it
 - Otherwise we found an element in the middle of the board which is already
 - filled so we move forward
 -}
solve :: Int -> [Int] -> IO ()
solve n board
    | board !! n == 0 = mapM_ (solve' board) $ availableElems n board
    | n == 80 = print board
    | otherwise = solve (n+1) board
    where solve' board x = do
            let newboard = replaceAtIndex n x board
            if n == 80 then do
                print newboard
            else do
                solve (n+1) newboard
