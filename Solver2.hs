module Solver2 (solve) where

import KakuroCommon
import Data.List
import Data.Maybe


type RowStripLoc = (Int, Int, Int)

stripX :: RowStripLoc -> Int
stripX (x,_,_) = x

stripY :: RowStripLoc -> Int
stripY (_,y,_) = y

stripLen :: RowStripLoc -> Int
stripLen (_,_,l) = l

-- solve p returns Nothing if it is not solvable or returns Just p0 where p0 is the solved 
-- version of the board, assumes board given is valid 

solve :: Puzzle -> Maybe Puzzle
solve p 
  | filled p = Just p
  | otherwise = solveAll (nextPuzzles p) 

solveAll :: [Puzzle] -> Maybe Puzzle
solveAll [] = Nothing 
solveAll (p:ps) = if isNothing $ solve p then solveAll ps else solve p

--nextPuzzles :: Puzzle -> [Puzzle]
nextPuzzles p 
  | isNothing mIncompleteLoc = []
  | otherwise = filter validColumns [setRowStrip p strip incompleteLoc | strip <- groupPossibilities]
  where 
    mIncompleteLoc = firstEmptyRowStrip p
    incompleteLoc = fromJust mIncompleteLoc
    incompleteX = stripX incompleteLoc 
    incompleteY = stripY incompleteLoc
    incompleteLen = stripLen incompleteLoc
    currGroup = getRowStrip p incompleteLoc
    Clue (_,clue) =  squareAt p (incompleteX - 1, incompleteY)
    groupPossibilities = fullGroupPossibilities clue currGroup
    stripCols puz = take incompleteLen (drop incompleteX (columns puz))
    validColumns = areColsValid . stripCols 


areColsValid :: [[Square]] -> Bool
areColsValid [] = True
areColsValid (c:cs) = isColValid c && areColsValid cs

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



firstEmptyRowStrip :: Puzzle -> Maybe RowStripLoc
firstEmptyRowStrip p
  | isNothing fEmpty = Nothing
  | otherwise = Just (startX, emptyY, endX - startX + 1)
  where 
    fEmpty = firstEmpty p
    emptyX = fst (fromJust fEmpty)
    emptyY = snd (fromJust fEmpty)
    findStart :: (Int, Int) -> Int
    findStart (x,y) = if x > 0 && isWritable (squareAt p (x-1, y)) then findStart (x-1,y) else x 
    findEnd :: (Int, Int) -> Int
    findEnd (x,y) = if x < (length (columns p)) - 1 && isWritable (squareAt p (x+1, y)) then findEnd (x+1,y) else x
    startX = findStart (emptyX, emptyY)
    endX = findEnd (emptyX, emptyY)

getRowStrip :: Puzzle -> RowStripLoc -> [Square]
getRowStrip _ (_,_,0) = []
getRowStrip p (x,y,l) = (squareAt p (x,y)):(getRowStrip p (x+1,y,l-1))

setRowStrip :: Puzzle -> [Square] -> RowStripLoc -> Puzzle
setRowStrip p [] _ = p 
setRowStrip p _ (_,_,0) = p
setRowStrip p (s:ss) (x,y,l) = setRowStrip (setSquare p s (x,y)) ss (x+1,y,l-1)

fullGroupPossibilities :: Int -> [Square] -> [[Square]]
fullGroupPossibilities c s = map (map toEntry) intAnswer
  where 
    intValues = [entryVal e | e <- s]
    intAnswer = intGroupPossibilities c intValues   

 ---- intGroupPossibilities :: Int -> [Int] -> [[Int]]
 --intGroupPossibilities c s = if numEmpty == 0 then [s] else foldl (++) (map (intGroupPossibilities c poss) nextPossibilities) []
 --  where 
 --    entryValues = deleteAll 0 s
 --    newClue = c - sum entryValues
 --    numEmpty = length s - length entryValues
 --    changeFirst' l x y = changeFirst x y l
 --    nextPossibilities = map (changeFirst' s 0) (possibilities c numEmpty)

intGroupPossibilities :: Int -> [Int] -> [[Int]]
intGroupPossibilities c s = map (\l -> changeWith 0 l s) noRepeatPossibilities 
  where
    entryValues = deleteAll 0 s
    newClue = c - sum entryValues
    numEmpty = length s - length entryValues
    emptyPossibilities = intGroupPossibilities' newClue numEmpty
    noRepeatPossibilities = filter (not . containsAny entryValues) emptyPossibilities

intGroupPossibilities' :: Int -> Int -> [[Int]]
intGroupPossibilities' c n = foldl (++) [] possUnflattened
  where 
    unfiltUniquePoss = permute n (possibilities c n)
    uniquePoss = filter (\x -> sum x == c) unfiltUniquePoss
    possUnflattened = map permutations uniquePoss

containsAny :: Eq a => [a] -> [a] -> Bool
containsAny check list = length (intersect check list) > 0

changeFirst :: Eq a => a -> a -> [a] -> [a]
changeFirst _ _ [] = []
changeFirst x y (z:t)
  | x == z = y:t 
  | otherwise = z:(changeFirst x y t)

changeWith :: Eq a => a -> [a] -> [a] -> [a]
changeWith _ _ [] = []
changeWith _ [] zt = zt
changeWith x (y:yt) (z:zt)
  | x == z = y:(changeWith x yt zt)
  | otherwise = z:(changeWith x (y:yt) zt)

possibilities :: Int -> Int -> [Int]
possibilities c n = foldl union [] allPerms
  where 
    allPerms = filter (\x -> sum x == c) $ permute n [1..9]

permute :: (Num a, Eq a) => a -> [b] -> [[b]]
permute 0 _ = [[]]
permute n l = [x:xs | x:xs' <- tails l, xs <- permute (n-1) xs']
