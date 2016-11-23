module Sudoku where

import Test.QuickCheck
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku ([ [ Nothing | x <- [1..9] ] | y <- [1..9] ])

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku r) | (length r) == 9 &&
                      and ([((length (r !! n)) == 9) | n <- [0..8]]) &&
                      (all (validSpace) (concat r)) = True

--Checks if a Maybe Int is either between 1 and 9 or nothing.
validSpace :: Maybe Int -> Bool
validSpace (Just n) = n > 0 && n < 10
validSpace Nothing  = True

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku r) = all (/= Nothing) (concat r)

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku r) = mapM_ putStrLn (map (listToCharList) r)
                            
--given a maybe int, returns the desired char.
valueToChar :: Maybe Int -> Char
valueToChar (Just n) = chr (48 + n)
valueToChar Nothing = '.'

listToCharList :: [Maybe Int] -> [Char]
listToCharList list = map valueToChar list

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------
