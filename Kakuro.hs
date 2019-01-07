module Kakuro where

import Control.Exception
import Data.List
import Data.Maybe
import Display
import KakuroCommon
import Solver2
import System.CPUTime

renderPuzzle :: Puzzle -> IO ()
renderPuzzle = puzzleWindow

puzzleToString :: Puzzle -> String
puzzleToString p = concatMap drawRow (rows p) ++ drawLastLine (size p)

-- Outputs a string representation of a puzzle that is readable
drawPuzzle :: Puzzle -> IO ()
drawPuzzle p = putStr (puzzleToString p)

-- Outputs a text file that contains a puzzle give it a file name, "filename.txt" and a puzzle
exportPuzzle :: FilePath -> Puzzle -> IO ()
exportPuzzle name p =
  writeFile name (concatMap drawRow (rows p) ++ drawLastLine (size p))

-- This timing stuff is taken directly from the Haskell website, worked better than importing code by hand
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
  startTime <- getCPUTime
  action arg
  finishTime <- getCPUTime
  return $ fromIntegral (finishTime - startTime) / 1000000000000

timePuzzle :: (Fractional c) => (a -> b) -> a -> IO c
timePuzzle f = timeIt (\x -> f x `seq` return ())

readSolveAndExport :: FilePath -> FilePath -> IO ()
readSolveAndExport name ename = do
  p <- readIOPuzzle name
  solveAndExport ename p

-- Takes a file name and a puzzle and solves the puzzle and exports it.
solveAndExport :: FilePath -> Puzzle -> IO ()
solveAndExport name p = do
  putStrLn "Starting to solve..."
  let rp = fromMaybe p (solve p)
  writeFile name (puzzleToString rp)
  putStrLn "Solved and exported."

-- Takes in a filename and returns an IO puzzle              
readIOPuzzle :: FilePath -> IO Puzzle
readIOPuzzle name = do
  content <- readFile name
  let linesOfFiles = lines content
  let p = convertStrings linesOfFiles
  let p1 = Puzzle p
  return p1

-- Draws an IO puzzle
drawIOPuzzle :: IO Puzzle -> IO ()
drawIOPuzzle p = p >>= drawPuzzle

solveIOPuzzle :: Monad m => m Puzzle -> m Puzzle
solveIOPuzzle = fmap solvePuzzleOrEmpty

solvePuzzleOrEmpty :: Puzzle -> Puzzle
solvePuzzleOrEmpty p =
  if isNothing solved
    then Puzzle []
    else fromJust solved
  where
    solved = solve p

--Helper to convert a list of strings into a list of lists of squares
convertStrings :: [String] -> [[Square]]
convertStrings = map stringToSquare

--Helper to convert a string into a list of squares
stringToSquare :: String -> [Square]
stringToSquare str = map read (words str)

-- Returns a string representation of a row of squares in a puzzle
drawRow :: [Square] -> String
drawRow r = concatMap (`drawTextLine` r) [0 .. 3]

-- Returns a string representation final line of size s (same as first line)
drawLastLine :: Int -> String
drawLastLine s = concat (replicate s "+---") ++ "+\n"

-- returns a string for each of the 4 rows of a cell in a puzzle (which depending on the first arg)
drawTextLine :: Int -> [Square] -> String
drawTextLine 0 r = drawLastLine (length r)
drawTextLine n r = concatMap (cellLine n) r ++ "|\n"

-- Helper function for drawTextLine to draw a line of an individual cell     
cellLine :: (Eq a, Num a) => a -> Square -> String
cellLine 0 _ = "+---"
cellLine _ Empty = "|   "
cellLine _ Blocked = "|xxx"
cellLine 2 (Entry x) = "| " ++ show x ++ " "
cellLine _ (Entry _) = "|   "
cellLine 1 (Clue (_, h)) =
  if h == 0
    then "|\\  "
    else take 4 ("|\\" ++ show h ++ " ")
cellLine 2 (Clue (_, _)) = "| \\ "
cellLine 3 (Clue (v, _)) =
  if v == 0
    then "|  \\"
    else take 3 ("|" ++ show v ++ " ") ++ "\\"
--TEST CASES--
-- This sets p1 to a solved 5x5 puzzle:
-- let p1 = Puzzle [[Blocked,Clue(23,0),Clue(22,0),Blocked,Blocked],[Clue(0,16),Entry 9, Entry 7,Clue(7,0),Clue(22,0)],[Clue(0,20),Entry 8,Entry 6,Entry 1,Entry 5],[Clue(0,25),Entry 6,Entry 9,Entry 2,Entry 8],[Blocked,Blocked,Clue(0,13),Entry 4,Entry 9]]
-- This sets p2 to the empty version of p1:
-- let p2 = Puzzle [[Blocked, Clue(23,0),Clue(22,0),Blocked,Blocked],[Clue(0,16),Empty,Empty,Clue(7,0),Clue(22,0)],[Clue(0,20),Empty,Empty,Empty,Empty],[Clue(0,25),Empty,Empty,Empty,Empty],[Blocked,Blocked,Clue(0,13),Empty,Empty]]
-- This sets p3 to an incorrectly solved version of p1: 
-- let p3 = Puzzle [[Blocked,Clue(23,0),Clue(22,0),Blocked,Blocked],[Clue(0,16),Entry 8, Entry 7,Clue(7,0),Clue(22,0)],[Clue(0,20),Entry 8,Entry 6,Entry 1,Entry 5],[Clue(0,25),Entry 6,Entry 9,Entry 2,Entry 8],[Blocked,Blocked,Clue(0,13),Entry 4,Entry 9]]
-- This sets p4 to a partially solved 6x6 puzzle: 
-- let p4 = Puzzle [[Blocked, Clue (20,0), Clue (12,0), Clue (16,0), Blocked, Blocked],[Clue (0, 23), Empty, Empty, Empty, Clue (29, 0), Blocked], [Clue (0,27),Empty,Empty,Empty,Empty,Clue (8,0)],[Clue (0,4), Empty, Empty, Clue (9,8), Empty, Empty], [Blocked, Clue (0,23),Entry 2,Entry 8,Empty,Empty],[Blocked,Blocked,Clue (0,9),Entry 1,Entry 5, Entry 3]]
-- Now try: 
-- drawPuzzle p1
-- solved p1
-- drawPuzzle p2
-- solved p2
-- p1 == fromJust . solve $ p2
-- drawPuzzle p3
-- solved p3
-- drawPuzzle p4
-- let p5 = fromJust . solve $ p4
-- drawPuzzle p5
-- The following reads a puzzle from puzzletest.txt, draws it, then draws the solution: 
-- let p6 = readIOPuzzle "puzzletest.txt" 
-- drawIOPuzzle p6
-- let p7 = solveIOPuzzle p6
-- drawIOPuzzle p7 
-- This code does the same in 1 line except it outputs the text to out.txt:
-- readSolveAndExport "puzzletest.txt" "out.txt"
-- Observe that the printed puzzle is the same as the one in out.txt
-- Now try these and observe they make ghci hang for an extremely long time:
-- solveOld p2
-- solveOld p4
