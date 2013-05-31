module SudokuSolver(
 {-solve-}
{-, lineToBoard-}
{-, squareToBoard-}
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
getLineIndexes index = take 9 [startPos..]
    where startPos = index - (index `mod` 9)

{- Given an element's index on the board (between 0 and 80) return all indexes
 - on the same column in a 9x9 board
 -
 - The logic behind this:
 - If the user wants to retrieve the indexes on the same column with the index
 - 41 then he will get: [5,14..77]
 - this is because I filter out everything in [0..80] that doesn't have the same
 - reminder as the index when doing `mod` 9
 -}
getColumnIndexes :: Int -> Column
{-getColumnIndexes index = filter ((==) remainder . (flip mod) 9) [0..80]-}
getColumnIndexes index = filter (\x -> (x `mod` 9) == remainder) [0..80]
    where remainder = index `mod` 9

getBoxIndexes :: Int -> Box
getBoxIndexes index = [42] -- TODO

solve :: SudokuBoard -> SudokuBoard
solve board = [1,2] -- TODO
