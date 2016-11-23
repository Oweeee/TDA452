module Sudoku where

import Test.QuickCheck
import Data.Char
import System.FilePath
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku ([ [ Nothing | x <- [1..9] ] | y <- [1..9] ])

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku r) | (length r) == 9 &&
                      and ([((length (r !! n)) == 9) | n <- [0..8]]) &&
                      (all (validSpace) (concat r)) = True
                    | otherwise = False

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

-- Converts a list of Maybe Int to a list of Chars
listToCharList :: [Maybe Int] -> [Char]
listToCharList list = map valueToChar list

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku p | takeExtension p /= ".sud" = error "Invalid file type"
readSudoku p = do
        text <- readFile p
        return (Sudoku (map listToMaybeIntList (lines text)))

-- Converts a list of Chars to a list of a list of Maybe Int
listToMaybeIntList :: [Char] -> [Maybe Int]
listToMaybeIntList list = map charToValue list 

-- Converts a Char to a Maybe Int
charToValue :: Char -> Maybe Int
charToValue c | c' > 48 && c' < 58 = (Just (c'-48))
    where c' = (ord c)
charToValue '.' = Nothing
charToValue c | otherwise = error "invalid Char"

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1,numbers),(9, nothing)]
    where 
        numbers = elements [ Just a | a <- [1..9]]
        nothing = elements [Nothing]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)


-- type representing 9 cells in form of a row, column or 3x3 block
type Block = [Maybe Int]

-- checks if a block is valid, meaning it does not contain the same value
-- more than once
isOkayBlock :: Block -> Bool
isOkayBlock b = not (checkIfEqual x xs)
    where (x:xs) = sort b


-- Checks if two ints in a list are equal
checkIfEqual :: Maybe Int -> Block -> Bool 
checkIfEqual Nothing (x:xs)             = checkIfEqual x xs
checkIfEqual n []                       = False  
checkIfEqual n (x:xs)       | n == x    = True
                            | otherwise = checkIfEqual x xs

-- creates a list of all blocks in a sudoku
blocks :: Sudoku -> [Block]
blocks s = [a !! s| a <- [0..8]]
         ++[b !! (transpose s)| b <- [0..8]]
         ++[]
>>>>>>> f583c1c1df64e31cd1a7fab8a84bb52d15585db8
