--Sudoku.hs

--A program to solve sudoku-puzzles.

--datatype sudoku
data Sudoku = Sudoku { rows :: [[Maybe Int]] }

--function to create a sudoku-board with only blank cells.
allBlankSudoku :: Sudoku
Sudoku rows = [ [ Nothing | x <- [1..9] ] | x <- [1..9] ]

--function that makes sure the given type is a sudoku.
isSudoku :: Sudoku -> Bool
--TODO: add stuff
