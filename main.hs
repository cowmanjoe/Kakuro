import Data.Char
import Display
import Kakuro

data PuzzleAction
  = Print
  | Export FilePath
  | Display
  | Invalid

main :: IO ()
main = do
  putStrLn "Welcome to the kakuro solver!"
  putStrLn "What is the file name of the Kakuro?"
  fileName <- getLine
  putStrLn
    "What would you like to do with this puzzle? (print / export <filename> / display)"
  action <- getLine
  doAction (parseAction action) fileName

parseAction :: String -> PuzzleAction
parseAction input
  | null inputWords = stringToAction ("", Nothing)
  | otherwise = stringToAction (action, path)
  where
    inputWords = Prelude.words input
    action = map toLower $ head inputWords
    path =
      if action == "export"
        then Just $ inputWords !! 1
        else Nothing

stringToAction :: (String, Maybe FilePath) -> PuzzleAction
stringToAction ("print", Nothing) = Print
stringToAction ("export", Just path) = Export path
stringToAction ("display", Nothing) = Display
stringToAction _ = Invalid

doAction :: PuzzleAction -> FilePath -> IO ()
doAction Print importPath =
  drawIOPuzzle . solveIOPuzzle . readIOPuzzle $ importPath
doAction (Export path) importPath = readSolveAndExport importPath path
doAction Display importPath =
  (solveIOPuzzle . readIOPuzzle $ importPath) >>= puzzleWindow
doAction Invalid _ = putStrLn "Invalid action"
