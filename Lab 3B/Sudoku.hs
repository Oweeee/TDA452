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

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

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
blocks (Sudoku r) = r 
                  ++ transpose r
                  ++ createBox (take 3 r)
                  ++ createBox (take 3 (drop 3 r))
                  ++ createBox (drop 6 r)

-- helper function which creates 3x3 blocks.
createBox :: [[Maybe Int]] -> [Block]
createBox list =    transpose [concat (take 3 list')] 
                 ++ transpose [concat (take 3 (drop 3 list'))] 
                 ++ transpose [concat (drop 6 list')]
    where list' = transpose lengthist 

-- Checks that blocks is of correct length and that its elements
-- are of correct length.
prop_blockSize :: Sudoku -> Bool
prop_blockSize sudoku = (length (blockList) == 27) &&
                        (all (correctLength) blockList)
            where blockList = blocks sudoku

-- helper function to check if a list has length 9.
correctLength :: [Maybe Int] -> Bool
correctLength list = (length list == 9)

-- checks that all blocks in a sudoku does not contain the same value twice
isOkay :: Sudoku -> Bool
isOkay s = (isSudoku s) && (all (isOkayBlock) (blocks s))

-- tyoe which identifies the location of a cell.
type Pos = (Int,Int)

-- creates a list of all blank cell positions
blanks :: Sudoku -> [Pos]
blanks (Sudoku r) = allBlanksPos
  where 
      allCells = concat r
      allPos = [(x,y) | x <- [0..8], y <- [0..8]]
      cellPosList = zip allCells allPos
      allBlanksPos = allBlanks cellPosList
        where
          allBlanks :: [(Maybe Int, Pos)] -> [Pos]
          allBlanks ((Nothing, pos):[])  = [pos]
          allBlanks (((Just n), pos):[]) = []
          allBlanks ((Nothing, pos):xs)  = [pos] ++ allBlanks xs
          allBlanks (((Just n), pos):xs) = allBlanks xs
          
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) (x:xs) (0, value) = value:xs
(!!=) (x:xs) (i, value) = x:(!!=) xs ((i-1), value) 

-- updates a cell in a sudoku with a new given value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku (r:rs)) (0,y) value = (Sudoku (((!!=) r (y,value)):rs))
update (Sudoku (r:rs)) (x,y) value = update (Sudoku rs) ((x-1),y) value

{-
relevantBlocks :: [Block]
relevantBlocks = [a, b, c]
    where 
        a = (blocks example) !! 1
        b = (blocks example) !! 10
        c = (blocks example) !! 18
-}
-- returns a list of acceptable ints which can be inserted to a cell
candidates :: Sudoku -> Pos -> [Int]
candidates s p = candidates' s p 9
    where
        candidates' :: Sudoku -> Pos -> Int -> [Int]
        candidates' s p 1 = []
        candidates' s p i | testValue (update s p (Just i)) p = i: (candidates' s p (i-1))
                          | otherwise    = candidates' s p (i-1)    

testValue :: Sudoku -> Pos -> Bool
testValue s p = all isOkayBlock (relevantBlocks (blocks s) p)
    where 
        relevantBlocks :: [Block] -> Pos -> [Block]
        relevantBlocks b (x,y) = [(b !! x), (b !! (9+y)), (whatBox b (x,y))]
            where

                whatBox :: [Block] -> Pos -> Block
                whatBox b (x,y)
                    | y < 3 = b !! (17+(div x 3))
                    | y > 5 = b !! (23+(div x 3))
                    | otherwise = b !! (20+(div x 3)) 
            