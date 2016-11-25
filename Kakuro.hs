module Kakuro where

import Data.List

-- Represents a square-shaped Kakuro puzzle 
-- Size of rows and each element of rows must be size

data Puzzle = Puzzle {  size :: Int,
						rows :: [[Square]]  	
					 }

-- Blocked indicates this square does not need to be filled in
-- Empty indicates this square is not filled in, and needs to be to solve the puzzle
-- Clue (Int, Int) is a clue square where the first of the tuple is the vertical clue and  
-- the second of the tuple is the horizontal clue. If one of the tuples is 0, then there
-- is no horizontal or vertical clue depending on which one is 0
-- Entry Int is an entry into an empty space
data Square = Blocked | Empty | Clue (Int, Int) | Entry Int deriving (Show)

-- row p x returns row x of puzzle p
row :: Puzzle -> Int -> [Square]
row p x = (rows p) !! x 

-- column p x returns column x of puzzle p
column :: Puzzle -> Int -> [Square]
column p x = [l !! x | l <- rows p] 

-- columns p returns a list of all the columns of p
columns :: Puzzle -> [[Square]]
columns p = map (column p) [0..((size p) - 1)]

-- isEntry s returns true if square s is an Entry
isEntry :: Square -> Bool 
isEntry (Entry _) = True 
isEntry _ = False 

-- isEmpty s returns true if squre s is empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

--isWritable s returns true if square s is an entry or an empty square
isWritable :: Square -> Bool 
isWritable s = isEntry s || isEmpty s

-- entryVal returns the value of an entry, or 0 if it is empty
entryVal (Entry x) = x
entryVal Empty = 0


-- rowSolved l is true if the given row is solved 
rowSolved :: [Square] -> Bool
rowSolved [] = True
rowSolved (Blocked:xs) = rowSolved xs
rowSolved (Clue (_,c):xs) = notElem 0 entryValues && (sum entryValues == c) && noRepeats && rowSolved remainder
	where 
		entryValues = [entryVal e | e <- takeWhile isWritable xs]
		noRepeats = ((length (nub entryValues)) == (length entryValues))
		remainder = dropWhile isEntry xs 
	
-- colSolved l is true if the given column is solved 
colSolved :: [Square] -> Bool 
colSolved [] = True 
colSolved (Blocked:xs) = colSolved xs
colSolved (Clue (c,_):xs) = notElem 0 entryValues && (sum entryValues == c) && noRepeats && colSolved remainder
	where 
		entryValues = [entryVal e | e <- takeWhile isWritable xs]
		noRepeats = ((length (nub entryValues)) == (length entryValues))
		remainder = dropWhile isEntry xs 


solved :: Puzzle -> Bool
solved p = (and [rowSolved r | r <- rows p]) && (and [colSolved c | c <- columns p])