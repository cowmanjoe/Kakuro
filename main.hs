
import Kakuro
import Data.Char
import Graphics.UI.GLUT
import Data.Function

data PuzzleAction = Print | Export FilePath | Display | Invalid

main :: IO ()
main = do
  putStrLn "Welcome to the kakuro solver!" 
  putStrLn "What is the file name of the Kakuro?" 
  fileName <- getLine
  putStrLn "What would you like to do with this puzzle? (print / export <filename> / display)" 
  action <- getLine
  
  doAction (parseAction action) fileName  


 

parseAction :: String -> PuzzleAction
parseAction input  
  | null inputWords = stringToAction ("", Nothing)
  | otherwise = stringToAction (action, path) 
  where inputWords = Prelude.words input
        action = map toLower $ head inputWords
        path   = if action == "export" then Just $ inputWords !! 1 else Nothing


stringToAction :: (String, Maybe FilePath) -> PuzzleAction
stringToAction ("print", Nothing) = Print
stringToAction ("export", Just path) = Export path
stringToAction ("display", Nothing) = Display
stringToAction _ = Invalid

doAction :: PuzzleAction -> FilePath -> IO ()
doAction Print importPath = drawIOPuzzle . solveIOPuzzle . readIOPuzzle $ importPath
doAction (Export path) importPath = readSolveAndExport importPath path 
doAction Display importPath = (solveIOPuzzle . readIOPuzzle $ importPath) >>= puzzleWindow 
doAction Invalid _ = putStrLn "Invalid action"


puzzleWindow :: Puzzle -> IO ()
puzzleWindow p = do 
  getArgsAndInitialize
  _window <- createWindow "Hello World!" 
  displayCallback $= displayPuzzle p 
  clearColor $= Color4 1 1 1 1
  mainLoop 

displayPuzzle  :: Puzzle -> DisplayCallback 
displayPuzzle p = do  
  clear [ ColorBuffer ]
  color $ Color3 0 0 (0 :: GLfloat) 
  renderPuzzle p
  flush

renderPuzzle :: Puzzle -> IO ()
renderPuzzle p = do 
  renderCells p
  renderGrid (size p)


renderCells :: Puzzle -> IO () 
renderCells p = mapM_ (\row -> renderRow ((rows p) !! row) (size p) row) [0..size p - 1] 


renderRow :: [Square] -> Int -> Int -> IO ()
renderRow squares size row = mapM_ (\col -> renderCell (squares !! col) size row col) [0..size - 1]


renderCell :: Square -> Int -> Int -> Int -> IO() 
renderCell square size row col 
    | isEmpty $ square = return ()
    | square == Blocked = renderBlocked size row col 
    | otherwise = return ()

renderBlocked :: Int -> Int -> Int -> IO ()
renderBlocked size row col = do 
  color3f 0 0 0 
  renderFromTuple Quads box
    where 
      box = [
        (left, top, 0), 
        (left, bottom, 0), 
        (right, bottom, 0), 
        (right, top, 0)]
      left = fromRational $ 2*(row /. size) - 1
      top = fromRational $ -(2*col /. size - 1)
      right = fromRational $ 2*row /. size - 1 + 2 /. size 
      bottom = fromRational $ -(2*col /. size - 1 + 2 /. size) 


(/.) = (/) `on` fromIntegral

renderFromTuple :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderFromTuple mode vertices = renderPrimitive mode $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) vertices



renderGrid :: Integral a => a -> IO ()
renderGrid n = renderPrimitive Lines $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (gridLines n)

gridLines :: Integral a => a -> [(GLfloat, GLfloat, GLfloat)]
gridLines n = horizontalLines n ++ verticalLines n


horizontalLines :: Integral a => a -> [(GLfloat, GLfloat, GLfloat)]
horizontalLines n = interleave 
    [(-1, 2.0 * y / (fromIntegral n) - 1, 0) | y <- map fromIntegral [0..n]] 
    [(1, 2.0 * y / (fromIntegral n) - 1, 0) | y <- map fromIntegral [0..n]]

verticalLines :: Integral a => a -> [(GLfloat, GLfloat, GLfloat)] 
verticalLines n = interleave 
    [(2.0*x/(fromIntegral n) - 1, -1, 0) | x <- map fromIntegral [0..n]] 
    [(2.0*x/(fromIntegral n) - 1, 1, 0) | x <- map fromIntegral [0..n]]


interleave :: [a] -> [a] -> [a]
interleave xs [] = xs 
interleave [] xs = xs 
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)


color3f r g b = color $ Color3 r g (b :: GLfloat)
