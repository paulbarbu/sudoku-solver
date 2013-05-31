module SudokuSolver(
 solve
, lineToBoard
, squareToBoard
)
where

import Data.Char(digitToInt)
import Data.List(nubBy)

type SudokuBoard = [Int]
type Line = [Int]
type Column = [Int]
type Box = [Int]

-- Transforms something like "123" in [1,2,3]
lineToBoard :: String -> SudokuBoard
lineToBoard = map digitToInt

-- Transforms something like "1 2 3\n4 5 6" in [1,2,3,4,5,6]
squareToBoard :: String -> SudokuBoard
squareToBoard contents = map read $ concat . map words $ lines contents

{- Given an element's index on the board (between 0 and 80) return all indexes
 - on the same line in a 9x9 board
 -
 - The logic behind this:
 - If the user wants to retrieve the indexes on the same line with the index 41
 - then he will get: [36,37..44]
 - this is because the start position (36) is 41 - 5.
 - 5 being 41 `mod` 9. So I take 9 elements starting with startPos.
 -}
getLineIndexes :: Int -> Line
getLineIndexes index = take 9 [first..]
    where first = index - (index `mod` 9)

{- Given an element's index on the board (between 0 and 80) return all indexes
 - on the same column in a 9x9 board
 -
 - If the user wants to retrieve the indexes on the same column with the index
 - 41 then he will get: [5,14..77]
 -
 - The logic behind the first implementation (filter):
 - I filter out everything in [0..80] that doesn't have the same remainder as the
 - index when doing `mod` 9
 -
 - The logic behind the second implementation:
 - I calculate the start element and then generate the rest of the column by
 - adding 9 to the previous element
 -}
getColumnIndexes :: Int -> Column
getColumnIndexes index = take 9 [first,first+9 ..]
    where first = index `mod` 9

getBoxIndexes :: Int -> Box
getBoxIndexes index = take 3 [start..] -- TODO columns
    where start = index - (index `mod` 3)

solve :: SudokuBoard -> SudokuBoard
solve board = [1,2] -- TODO
