module Kakuro where


data Puzzle = Puzzle {  size :: Int,
						rows :: [[Square]]  
						
					 }

-- Blank indicates this square does not need to be filled in
-- Empty indicates this square is not filled in, and needs to be to solve the puzzle
-- 
data Square = Blank | Empty | Clue (Int, Int) | Entry Int


--columns :: Puzzle -> [[Square]]
--columns p = []


--solved :: Puzzle -> Bool
