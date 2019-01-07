module Display (puzzleWindow) where 

import KakuroCommon 
import Graphics.UI.GLUT 
import Data.Function 


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
  color3f 0 0 0
  renderPuzzle p
  flush

renderPuzzle :: Puzzle -> IO ()
renderPuzzle p = do 
  renderCells p
  renderGrid (size p)


renderCells :: Puzzle -> IO () 
renderCells p = mapM_ (\row -> renderRow (rows p !! row) (size p) row) [0..size p - 1] 


renderRow :: [Square] -> Int -> Int -> IO ()
renderRow squares size row = mapM_ (\col -> renderCell (squares !! col) size row col) [0..size - 1]


renderCell :: Square -> Int -> Int -> Int -> IO ()
renderCell Empty _ _ _ = return()
renderCell Blocked size row col = renderBlocked size row col
renderCell (Entry n) size row col = renderEntry n size row col
renderCell (Clue c) size row col = renderClue c size row col
    

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

renderEntry :: Int -> Int -> Int -> Int -> IO ()
renderEntry entry size row col = do
  color3f 0 0 0
  renderCenteredText TimesRoman24 textPosition (show entry)
    where 
      textPosition = centerText (cellPosition size row col)
      centerText (x, y, z) = (x + fromRational (1 /. size), y - fromRational (1 /. size), z)

renderClue :: (Int, Int) -> Int -> Int -> Int -> IO ()
renderClue (v, h) size row col = do 
  color3f 0 0 0
  renderFromTuple Lines diagLine
  --currentRasterPosition $= vertex4 vCluePosition 
  --renderString TimesRoman24 vClue
  renderCenteredText TimesRoman24 vCluePosition vClue
  --currentRasterPosition $= vertex4 hCluePosition
  --renderString TimesRoman24 hClue
  renderCenteredText TimesRoman24 hCluePosition hClue
  renderClueTriangle (v, h) size row col
    where
      diagLine = [cellPosition size row col, cellPosition size (row + 1) (col + 1)]
      vClueTransform (x, y, z) = (x + 3 * quarterCell, y - quarterCell, z)
      hClueTransform (x, y, z) = (x + quarterCell, y - 3*quarterCell, z)
      vCluePosition = vClueTransform $ cellPosition size row col
      hCluePosition = hClueTransform $ cellPosition size row col 
      quarterCell = fromRational (2 /. size / 4)
      getClue c = if c >= 1 then show c else ""
      vClue = getClue v
      hClue = getClue h

renderClueTriangle :: (Int, Int) -> Int -> Int -> Int -> IO ()
renderClueTriangle (v, h) size row col 
  | v >= 1 && h >= 1 = return () 
  | v >= 1 = renderFromTuple Triangles topTriangle 
  | h >= 1 = renderFromTuple Triangles bottomTriangle
  | otherwise = error "Neither clue entries >= 1" 
    where
      topTriangle = [cellPosition size row col, cellPosition size row (col + 1), cellPosition size (row + 1) (col + 1)]
      bottomTriangle = [cellPosition size row col, cellPosition size (row + 1) col, cellPosition size (row + 1) (col + 1)]

cellPosition :: Int -> Int -> Int -> (GLfloat, GLfloat, GLfloat) 
cellPosition size row col = (fromRational $ 2*(row /. size) - 1, fromRational $ -(2*col /. size - 1), 0)

renderFromTuple :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderFromTuple mode vertices = renderPrimitive mode $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) vertices

renderCenteredText :: Font a => a -> (GLfloat, GLfloat, GLfloat) -> String -> IO () 
renderCenteredText font (x, y, z) text = do 
  size <- get windowSize
  textWidth <- stringWidth font text 
  textHeight <- fontHeight font 
  currentRasterPosition $= vertex4 (position size (fromIntegral textWidth) (fromIntegral . round $ textHeight)) 
  renderString font text
    where 
      widthInUnits scrWidth tWidth = 2 * tWidth /. scrWidth 
      heightInUnits scrHeight tHeight = 2 * tHeight / scrHeight 
      xpos scrWidth tWidth = x - fromRational (widthInUnits scrWidth tWidth) / 2
      ypos scrHeight tHeight = y - fromRational (widthInUnits scrHeight tHeight) / 2
      position (Size w h) tWidth tHeight = (xpos (fromIntegral w) tWidth, ypos (fromIntegral h) tHeight, z) 

renderGrid :: Int -> IO ()
renderGrid n = do
  color3f 0 0 0
  renderPrimitive Lines $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (gridLines n)

gridLines :: Int -> [(GLfloat, GLfloat, GLfloat)]
gridLines n = horizontalLines n ++ verticalLines n


horizontalLines :: Int -> [(GLfloat, GLfloat, GLfloat)]
horizontalLines n = interleave 
    [(-1, 2.0 * y / fromIntegral n - 1, 0) | y <- map fromIntegral [0..n]] 
    [(1, 2.0 * y / fromIntegral n - 1, 0) | y <- map fromIntegral [0..n]]

verticalLines :: Int -> [(GLfloat, GLfloat, GLfloat)] 
verticalLines n = interleave 
    [(2*x / fromIntegral n - 1, -1, 0) | x <- map fromIntegral [0..n]] 
    [(2*x / fromIntegral n - 1, 1, 0) | x <- map fromIntegral [0..n]]


interleave :: [a] -> [a] -> [a]
interleave xs [] = xs 
interleave [] xs = xs 
interleave (x:xs) (y:ys) = x:y:interleave xs ys


color3f r g b = color $ Color3 r g (b :: GLfloat)

(/.) = (/) `on` fromIntegral


vertex4 (x, y, z) = Vertex4 x y z 1
