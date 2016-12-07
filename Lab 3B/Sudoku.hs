module Sudoku where

import Test.QuickCheck
import Data.Char
import System.FilePath
import Data.List
import Data.Maybe

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
                  ++ getAllSquares (Sudoku r)

getAllSquares :: Sudoku -> [Block]
getAllSquares (Sudoku r) = 
                     createSquare (take 3 r)
                  ++ createSquare (take 3 (drop 3 r))
                  ++ createSquare (drop 6 r)

-- helper function which creates 3x3 blocks.
createSquare :: [[Maybe Int]] -> [Block]
createSquare list = [concat (transpose(take 3 list'))] 
                 ++ [concat (transpose(take 3 (drop 3 list')))] 
                 ++ [concat (transpose(drop 6 list'))]
    where list' = transpose list 

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
      allBlanksTupleList = filter (\(a,_) -> isNothing a) cellPosList
      allBlanksPos = map snd allBlanksTupleList

-- replaces a value in a list at an index          
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) (x:xs) (0, value) = value:xs
(!!=) (x:xs) (i, value) = x:(!!=) xs ((i-1), value) 

-- updates a cell in a sudoku with a new given value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku (r:rs)) (0,y) value = (Sudoku (((!!=) r (y,value)):rs))
update (Sudoku (r:rs)) (x,y) value = Sudoku (r:rs')
    where
        (Sudoku rs') = update (Sudoku rs) ((x-1),y) value

-- returns a list of acceptable ints which can be inserted to a cell
candidates :: Sudoku -> Pos -> [Int]
candidates s p = candidates' s p 9
    where
        candidates' :: Sudoku -> Pos -> Int -> [Int]
        candidates' s p 0 = []
        candidates' s p i | testValue (update s p (Just i)) p = 
                            i:(candidates' s p (i-1))
                          | otherwise    = candidates' s p (i-1)    
                          
-- checks if if a cell contains a legal value
testValue :: Sudoku -> Pos -> Bool
testValue s p = all isOkayBlock (relevantBlocks s p)
 
-- returns the 3 blocks containing the cell
relevantBlocks :: Sudoku -> Pos -> [Block]
relevantBlocks s p = [(getRow s p), (getColumn s p), (getSquare s p)]

getRow :: Sudoku -> Pos -> Block
getRow (Sudoku r) (x,y) = r !! x

getColumn :: Sudoku -> Pos -> Block
getColumn (Sudoku r) (x,y) = (transpose r) !! y

getSquare :: Sudoku -> Pos -> Block
getSquare s (x,y) = (l !! (div x 3)) !! (div y 3)
            where 
                allSquares = getAllSquares s
                l = [take 3 allSquares]
                 ++ [take 3 (drop 3 allSquares)]
                 ++ [drop 6 allSquares]
 
-- solves a sudoku         
solve :: Sudoku -> Maybe Sudoku
solve s | not (isOkay s) = Nothing
solve s = solve' s (blanks s)
  where
    solve' :: Sudoku -> [Pos] -> Maybe Sudoku
    solve' sud [] = Just sud
    solve' sud (p:ps) = listToMaybe (catMaybes sudList)
      where
        sudList = [solve' (update sud p (Just c)) ps | c <- cs]
        cs = candidates sud p

-- reads a sudoku from a file, solves it and prints it
readAndSolve :: FilePath -> IO ()
readAndSolve path = 
            do
                sud <- readSudoku path
                let solvedSud = solve sud
                if isNothing solvedSud
                    then putStrLn "no solution"
                    else printSudoku (fromJust solvedSud)

-- checks if a sudoku is a valid solution of another sudoku                    
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol sud = ((isOkay sol) && (isOkay sud) && (isSolved sol) 
                        && (and (zipWith(\x y -> x == y || isNothing y) 
                        (concat (rows sol)) (concat (rows sud)))))

--property which checks if the solve method is sound
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isJust (solve sud) ==> 
                      (isSolutionOf (fromJust (solve sud)) sud)
            
                       

