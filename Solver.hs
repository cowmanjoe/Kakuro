module Solver (
  solve
) where

import Data.List
import Data.Maybe
import KakuroCommon

-- rowSolved l is true if the given row is solved 
rowSolved :: [Square] -> Bool
rowSolved [] = True
rowSolved (Blocked:xs) = rowSolved xs
rowSolved (Clue (_,c):xs) = notElem 0 squareValues && (sum squareValues == c) && noRepeats && rowSolved remainder
  where 
    squareValues = [entryVal e | e <- takeWhile isWritable xs]
    noRepeats = length (nub squareValues) == length squareValues
    remainder = dropWhile isWritable xs 
  
-- colSolved l is true if the given column is solved 
colSolved :: [Square] -> Bool 
colSolved [] = True 
colSolved (Blocked:xs) = colSolved xs
colSolved (Clue (c,_):xs) = notElem 0 squareValues && (sum squareValues == c) && noRepeats && colSolved remainder
  where 
    squareValues = [entryVal e | e <- takeWhile isWritable xs]
    noRepeats = length (nub squareValues) == length squareValues
    remainder = dropWhile isWritable xs 

-- Functions with "Old" appended to the end were the naive, slower versions of the functions 
-- I wrote before implementing arc and domain consistency wherever possible     

-- solvedOld p returns true if p is a solvedOld puzzle (much slower than solve)
solvedOld :: Puzzle -> Bool
solvedOld p = and [rowSolved r | r <- rows p] && and [colSolved c | c <- columns p]


solveOld :: Puzzle -> Maybe Puzzle
solveOld p  
  | solvedOld p = Just p
  | otherwise = solveAllOld (nextPuzzlesOld p)

  
solveAllOld :: [Puzzle] -> Maybe Puzzle
solveAllOld [] = Nothing
solveAllOld (p:ps) = if isNothing (solveOld p) then solveAllOld ps else solveOld p


nextPuzzlesOld :: Puzzle -> [Puzzle]
nextPuzzlesOld p = if isNothing $ firstEmpty p then [] else map (\x -> setSquare p (Entry x) (fromJust . firstEmpty $ p)) [1..9]

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
solveAll (p:ps) = if isNothing $ solve p then solveAll ps else solve p



-- nextPuzzles p returns a list of puzzles with the first Empty in p replaced by Entry 1 to 9
nextPuzzles :: Puzzle -> [Puzzle]
nextPuzzles p = if isNothing varSquare then [] else filter validNumber (map (\x -> setSquare p (Entry x) (fromJust . firstEmpty $ p)) [1..9])
  where 
    varSquare = firstEmpty p
    squareX = maybe (-1) fst varSquare
    squareY = maybe (-1) snd varSquare
    validNumber p = isRowValid (row p squareY) && isColValid (column p squareX)


-- isValid p returns false if we can rule this puzzle out as a possible partial 
-- (or full) solution, and true otherwise 
isValid :: Puzzle -> Bool
isValid p = and [isRowValid r | r <- rows p] && and [isColValid c | c <- columns p]

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
        noRepeats = length (nub entries) == length entries
        filledIn = entries == s
        validSum = (filledIn && sum squareValues == c) || (not filledIn && sum squareValues < c)

-- groupPossibilities c s takes clue c and list of squares s and returns a list of lists of squares
-- where each item in the list is the list of possible squares for that cell 
-- Cells are required to be Empty or Entry i

groupPossibilities :: Int -> [Square] -> [[Square]]
groupPossibilities _ [] = []
groupPossibilities c s = map (map toEntry)  intAnswer
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



