module Kakuro where

import Data.List

-- Represents a square-shaped Kakuro puzzle 
-- Size of rows and each element of rows must be size

data Puzzle = Puzzle {  size :: Int,
						rows :: [[Square]]  	
					 }

-- Blank indicates this square does not need to be filled in
-- Empty indicates this square is not filled in, and needs to be to solve the puzzle
-- Clue (Int, Int) is a clue square where the first of the tuple is the vertical clue and  
-- the second of the tuple is the horizontal clue. If one of the tuples is 0, then there
-- is no horizontal or vertical clue depending on which one is 0
-- Entry Int is an entry into a blank space
data Square = Blank | Empty | Clue (Int, Int) | Entry Int deriving (Show)


row :: Puzzle -> Int -> [Square]
row p x = (rows p) !! x 

-- column p x returns column x of puzzle p
column :: Puzzle -> Int -> [Square]
column p x = [l !! x | l <- rows p] 

-- columns p returns a list of all the columns of p
columns :: Puzzle -> [[Square]]
columns p = map (column p) [0..((size p) - 1)]

