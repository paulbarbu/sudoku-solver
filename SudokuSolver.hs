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

lineToBoard :: String -> SudokuBoard
lineToBoard = map digitToInt

squareToBoard :: String -> SudokuBoard
squareToBoard contents = map read $ concat . map words $ lines contents

getBoardLine :: Int -> Line
getBoardLine index = take 9 [start..]
    where start = index - (index `mod` 9)

getBoardColumn :: Int -> Column
{-getBoardColumn index = filter ((==) remainder . (flip mod) 9) [0..80]-}
getBoardColumn index = filter (\x -> (x `mod` 9) == remainder) [0..80]
    where remainder = index `mod` 9

getBoardBox :: Int -> Box
getBoardBox index = [42] -- TODO

solve :: SudokuBoard -> SudokuBoard
solve board = [1,2] -- TODO
