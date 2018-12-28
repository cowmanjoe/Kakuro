module KakuroCommon (
  Puzzle(Puzzle,rows),
  Square(Blocked,Empty,Clue,Entry),
  size,
  row,
  column,
  columns,
  isEntry,
  isClue,
  toEntry,
  isWritable,
  entryVal,
  flattened,
  unflattened,  
  deleteAll,
  squareAt,
  firstEmpty,
  filled,
  replaceAt,
  setSquare
) where

import Data.Maybe
import Data.List

-- Represents an n x n Kakuro puzzle 
-- Size of rows and each element of rows must be equal         
newtype Puzzle = Puzzle { rows :: [[Square]] } deriving (Show,Eq)


-- Blocked indicates this square does not need to be filled in
-- Empty indicates this square is not filled in, and needs to be to solve the puzzle
-- Clue (Int, Int) is a clue square where the first of the tuple is the vertical clue and  
-- the second of the tuple is the horizontal clue. If one of the tuples is 0, then there
-- is no horizontal or vertical clue depending on which one is 0
-- Entry Int is an entry into an empty space
data Square = Blocked | Empty | Clue (Int, Int) | Entry Int deriving (Show, Eq, Read)

size :: Puzzle -> Int
size p = length (rows p)

-- row p x returns row x of puzzle p
row :: Puzzle -> Int -> [Square]
row p x = rows p !! x 

-- column p x returns column x of puzzle p
column :: Puzzle -> Int -> [Square]
column p x = [l !! x | l <- rows p] 

-- columns p returns a list of all the columns of p
columns :: Puzzle -> [[Square]]
columns p = map (column p) [0..(size p - 1)]

-- isEntry s returns true if square s is an Entry
isEntry :: Square -> Bool 
isEntry (Entry _) = True 
isEntry _ = False 

isClue :: Square -> Bool 
isClue (Clue _) = True
isClue _ = False

-- toEntry n returns Entry s 
toEntry :: Int -> Square
toEntry = Entry 

-- isEmpty s returns true if squre s is empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

--isWritable s returns true if square s is an entry or an empty square
isWritable :: Square -> Bool 
isWritable s = isEntry s || isEmpty s

-- entryVal returns the value of an entry, or 0 if it is empty
entryVal :: Square -> Int
entryVal (Entry x) = x
entryVal Empty = 0
entryVal _ = error "Invalid square for entryVal" 

-- flattened r returns the flattened list of rows of r
flattened :: [[Square]] -> [Square]
flattened = concat 

-- unflattened r s the unflattened rows of r with s elements per row
unflattened :: [Square] -> Int -> [[Square]]
unflattened [] _ = []
unflattened r s = take s r : unflattened (drop s r) s

-- deleteAll x lst deletes all occurrences of x in lst and returns the result 
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll e (x:xs) = if e == x then deleteAll e xs else x:deleteAll e xs

-- squareAt p x y returns is the square at column x and row y in Puzzle p
squareAt :: Puzzle -> (Int, Int) -> Square
squareAt p (x,y) = (rows p !! y) !! x

-- firstEmpty p returns the (x, y) coordinates of the first Empty square or Nothing if there is no Empty
-- This searches by left to right, then up and down, so (2,1) would be found before (0,5)
firstEmpty :: Puzzle -> Maybe (Int, Int)
firstEmpty p 
  | isNothing index = Nothing
  | otherwise = Just (fromJust index `mod` size p, fromJust index `div` size p)
  where
    index = elemIndex Empty (flattened (rows p))

-- filled returns true if the puzzle has no empty squares 
filled :: Puzzle -> Bool 
filled p = isNothing (firstEmpty p)

-- replaceAt i x lst returns lst with element at index i replaced with x
replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 e (_:xs) = e:xs
replaceAt i e (x:xs) = x:replaceAt (i - 1) e xs

--setSquare p s (x, y) returns the puzzle p with the square at x y set to s
setSquare :: Puzzle -> Square -> (Int, Int) -> Puzzle 
setSquare p s (x,y) = Puzzle (unflattened (replaceAt (y * size p + x) s (flattened (rows p))) (size p))


