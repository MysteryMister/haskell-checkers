module Constants where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss


--board size
shape::[Int]
shape = [1..8]

--display type
vision :: Display
vision = FullScreen

--background color
bgColor :: Color
bgColor = white

--frames per second
fps :: Int
fps = 60

--passive turn count limit
stalemateStepsLimit :: Int
stalemateStepsLimit = 15

--left board bound
leftBound :: Float 
leftBound = -1795

--bottom board bound
bottomBound :: Float 
bottomBound = -1360

--offset in X axis
xOffset :: Float 
xOffset = 390

--offset in Y axis
yOffset :: Float 
yOffset = 390

--left column's middle 
leftCenter :: Float 
leftCenter = -1370

--cell width
cellWidth :: Float
cellWidth = 117

--distance from board center to bound
boardRadius :: Float
boardRadius = 4 * cellWidth
