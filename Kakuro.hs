module Kakuro where

import Data.List
import Data.Maybe

-- Represents a size x size Kakuro puzzle 
-- Size of rows and each element of rows must be size
{-
data Puzzle' = Puzzle' {  size :: Int,
						rows :: [[Square]]  	
					 } deriving (Show,Eq)
-}
					 
data Puzzle = Puzzle { rows :: [[Square]] } deriving (Show, Eq)
--instance Show Puzzle where 
	
{-
-- returns a string for each of the 4 rows of a cell in a puzzle (which depending on the first arg)
drawTextLine :: Int -> [Square] -> String
drawTextLine 0 r = (foldl (++) "" (replicate (length r) "+---")) ++ "+\n"
drawTextLine 1 r = 
	where
		cellLine :: Int -> Square -> String
		cellLine 1 Empty = "|   "
		cellLine 1 (Entry x) = "| " ++ number x ++ " "
		cellLine 1 (Clue (v,h)) = if h == 0 then "|\\  " else ""
	
-}	
-- Blocked indicates this square does not need to be filled in
-- Empty indicates this square is not filled in, and needs to be to solve the puzzle
-- Clue (Int, Int) is a clue square where the first of the tuple is the vertical clue and  
-- the second of the tuple is the horizontal clue. If one of the tuples is 0, then there
-- is no horizontal or vertical clue depending on which one is 0
-- Entry Int is an entry into an empty space
data Square = Blocked | Empty | Clue (Int, Int) | Entry Int deriving (Show, Eq)


size :: Puzzle -> Int
size p = length (rows p)


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

-- toEntry n returns Entry s 
toEntry :: Int -> Square
toEntry x = Entry x

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
entryVal _ = error("Invalid square for entryVal") 

-- flattened r returns the flattened list of rows of r
flattened :: [[Square]] -> [Square]
flattened r = foldl (++) [] r

-- unflattened r s the unflattened rows of r with s elements per row
unflattened :: [Square] -> Int -> [[Square]]
unflattened [] _ = []
unflattened r s = (take s r):(unflattened (drop s r) s)

-- deleteAll x lst deletes all occurrences of x in lst and returns the result 
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll e (x:xs) = if (e == x) then deleteAll e xs else x:deleteAll e xs

-- squareAt p x y returns is the square at column x and row y in Puzzle p
squareAt :: Puzzle -> (Int, Int) -> Square
squareAt p (x,y) = ((rows p) !! y) !! x

-- firstEmpty p returns the (x, y) coordinates of the first Empty square or Nothing if there is no Empty
-- This searches by left to right, then up and down, so (2,1) would be found before (0,5)
firstEmpty :: Puzzle -> Maybe (Int, Int)
firstEmpty p 
	| index == Nothing = Nothing
	| otherwise = Just (fromJust index `mod` (size p), fromJust index `div` (size p))
	where
		index = elemIndex Empty (flattened (rows p))

-- filled returns true if the puzzle has no empty squares 
filled :: Puzzle -> Bool 
filled p = firstEmpty p == Nothing
-------------------------
-- replaceAt i x lst returns lst with element at index i replaced with x
replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 e (x:xs) = e:xs
replaceAt i e (x:xs) = x:replaceAt (i - 1) e xs

--setSquare p s (x, y) returns the puzzle p with the square at x y set to s
setSquare :: Puzzle -> Square -> (Int, Int) -> Puzzle 
setSquare p s (x,y) = Puzzle (unflattened (replaceAt (y * (size p) + x) s (flattened (rows p))) (size p))

-- rowSolved l is true if the given row is solved 
rowSolved :: [Square] -> Bool
rowSolved [] = True
rowSolved (Blocked:xs) = rowSolved xs
rowSolved (Clue (_,c):xs) = notElem 0 squareValues && (sum squareValues == c) && noRepeats && rowSolved remainder
	where 
		squareValues = [entryVal e | e <- takeWhile isWritable xs]
		noRepeats = ((length (nub squareValues)) == (length squareValues))
		remainder = dropWhile isWritable xs 
	
-- colSolved l is true if the given column is solved 
colSolved :: [Square] -> Bool 
colSolved [] = True 
colSolved (Blocked:xs) = colSolved xs
colSolved (Clue (c,_):xs) = notElem 0 squareValues && (sum squareValues == c) && noRepeats && colSolved remainder
	where 
		squareValues = [entryVal e | e <- takeWhile isWritable xs]
		noRepeats = ((length (nub squareValues)) == (length squareValues))
		remainder = dropWhile isWritable xs 

-- Functions with "Old" appended to the end were the naive, slower versions of the functions 
-- I wrote before implementing arc and domain consistency wherever possible 		

-- solvedOld p returns true if p is a solvedOld puzzle (much slower than solve)
solvedOld :: Puzzle -> Bool
solvedOld p = (and [rowSolved r | r <- rows p]) && (and [colSolved c | c <- columns p])


solveOld :: Puzzle -> Maybe Puzzle
solveOld p  
	| solvedOld p = Just p
	| otherwise = solveAllOld (nextPuzzlesOld p)

	
solveAllOld :: [Puzzle] -> Maybe Puzzle
solveAllOld [] = Nothing
solveAllOld (p:ps) = if (solveOld p) == Nothing then solveAllOld ps else solveOld p


nextPuzzlesOld :: Puzzle -> [Puzzle]
nextPuzzlesOld p = if (firstEmpty p) == Nothing then [] else map (\x -> (setSquare p (Entry x) (fromJust . firstEmpty $ p))) [1..9]

-- solved p returns true if the board is solved 
solved :: Puzzle -> Bool 
solved p = isValid p && filled p

-- solve p returns Nothing if it is not solvable or returns Just p0 where p0 is the solved 
-- version of the board, assumes board given is valid 

solve :: Puzzle -> Maybe Puzzle
solve p 
	| filled p = Just p
	| otherwise = solveAll (nextPuzzles p) 

solveAll :: [Puzzle] -> Maybe Puzzle
solveAll [] = Nothing 
solveAll (p:ps) = if (solve p) == Nothing then solveAll ps else solve p



-- nextPuzzles p returns a list of puzzles with the first Empty in p replaced by Entry 1 to 9
nextPuzzles :: Puzzle -> [Puzzle]
nextPuzzles p = if varSquare == Nothing then [] else filter validNumber (map (\x -> (setSquare p (Entry x) (fromJust . firstEmpty $ p))) [1..9])
	where 
		varSquare = firstEmpty p
		squareX = if varSquare == Nothing then -1 else fst . fromJust $ varSquare
		squareY = if varSquare == Nothing then -1 else snd . fromJust $ varSquare
		validNumber p = (isRowValid (row p squareY)) && (isColValid (column p squareX))


-- isValid p returns false if we can rule this puzzle out as a possible partial 
-- (or full) solution, and true otherwise 
isValid :: Puzzle -> Bool
isValid p = (and [isRowValid r | r <- rows p]) && (and [isColValid c | c <- columns p])

-- isRowValid returns false if we can rule this row out as a possible partial
-- solution, and true otherwise 
isRowValid :: [Square] -> Bool
isRowValid [] = True
isRowValid (Blocked:xs) = isRowValid xs
isRowValid (Clue (_,c):xs) = isValidGroup c group && isRowValid remainder
	where 
		group = takeWhile isWritable xs
		remainder = dropWhile isWritable xs 

		


-- isColValid returns false if we can rule this column out as a possible partial
-- solution, and true otherwise 
isColValid :: [Square] -> Bool
isColValid [] = True
isColValid (Blocked:xs) = isColValid xs
isColValid (Clue (c,_):xs) = isValidGroup c group && isColValid remainder
	where 
		group = takeWhile isWritable xs 
		remainder = dropWhile isWritable xs 
		
		
isValidGroup :: Int -> [Square] -> Bool 
isValidGroup c s = isPossible && noRepeats && validSum 
			where
				squareValues = map entryVal s 
				entries = deleteAll Empty s
				isPossible = [] `notElem` groupPossibilities c s
				noRepeats = (length (nub entries)) == (length entries)
				filledIn = entries == s
				validSum = ((filledIn && sum squareValues == c) || (not filledIn && sum squareValues < c))

-- groupPossibilities c s takes clue c and list of squares s and returns a list of lists of squares
-- where each item in the list is the list of possible squares for that cell 
-- Cells are required to be Empty or Entry i

groupPossibilities :: Int -> [Square] -> [[Square]]
groupPossibilities _ [] = []
groupPossibilities c s = map (\l -> map toEntry l) intAnswer
	where 
		squareValues = [entryVal e | e <- s]
		entryValues = deleteAll 0 squareValues 
		newClue = c - sum entryValues
		numEmpty = length s - length entryValues 
		intAnswer = map (\x -> if x == Empty then filter (`notElem` entryValues) (possibilities newClue numEmpty) else [entryVal x]) s

-- possibilities c n returns possibilities of each cell for a cell group of size n with a clue of c 
possibilities :: Int -> Int -> [Int]
possibilities c n = foldl union [] allPerms
	where 
		allPerms = filter (\x -> sum x == c) $ permute n [1..9]
		
-- Permutes the given values in a list of given length
permute :: (Num a, Eq a) => a -> [b] -> [[b]]
permute 0 _ = [[]]
permute n l = [x:xs | x:xs' <- tails l,xs <- permute (n-1) xs']

--TEST CASES--

-- This sets p1 to a solved 6x6 puzzle: 
-- let p1 = Puzzle [[Blocked, Clue (20,0), Clue (12,0), Clue (16,0), Blocked, Blocked],[Clue (0, 23), Entry 8, Entry 6, Entry 9, Clue (29, 0), Blocked], [Clue (0,27),Entry 9,Entry 3,Entry 7,Entry 8,Clue (8,0)],[Clue (0,4), Entry 3, Entry 1, Clue (9,8), Entry 7, Entry 1], [Blocked, Clue (0,23),Entry 2,Entry 8,Entry 9,Entry 4],[Blocked,Blocked,Clue (0,9),Entry 1,Entry 5, Entry 3]]

-- This sets p2 to p1 except for the last entry is altered so it is incorrect 
-- let p2 = Puzzle [[Blocked, Clue (20,0), Clue (12,0), Clue (16,0), Blocked, Blocked],[Clue (0, 23), Entry 8, Entry 6, Entry 9, Clue (29, 0), Blocked], [Clue (0,27),Entry 9,Entry 3,Entry 7,Entry 8,Clue (8,0)],[Clue (0,4), Entry 3, Entry 1, Clue (9,8), Entry 7, Entry 1], [Blocked, Clue (0,23),Entry 2,Entry 8,Entry 9,Entry 4],[Blocked,Blocked,Clue (0,9),Entry 1,Entry 5, Entry 2]]

-- This sets p3 to the unsolved version of p1 
-- let p3 = Puzzle [[Blocked, Clue (20,0), Clue (12,0), Clue (16,0), Blocked, Blocked],[Clue (0, 23),Empty, Empty, Empty, Clue (29, 0), Blocked], [Clue (0,27),Empty,Empty,Empty,Empty,Clue (8,0)],[Clue (0,4), Empty, Empty, Clue (9,8), Empty, Empty], [Blocked, Clue (0,23),Empty,Empty,Empty,Empty],[Blocked,Blocked,Clue (0,9),Empty,Empty, Empty]]

-- let p4 = Puzzle [[Blocked, Clue (20,0), Clue (12,0), Clue (16,0), Blocked, Blocked],[Clue (0, 23), Empty, Empty, Empty, Clue (29, 0), Blocked], [Clue (0,27),Empty,Empty,Empty,Empty,Clue (8,0)],[Clue (0,4), Empty, Empty, Clue (9,8), Empty, Empty], [Blocked, Clue (0,23),Entry 2,Entry 8,Empty,Empty],[Blocked,Blocked,Clue (0,9),Entry 1,Entry 5, Entry 3]]


-- Now try: 
-- column p1, 3
-- solved p1
-- solved p2