module Constants where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss


----константы----

--размер доски
shape::[Int]
shape = [1..8]

--тип экрана
vision :: Display
vision = FullScreen

--цвет фона
bgColor :: Color
bgColor = white

--ФПС
fps :: Int
fps = 60

--число "пассивных" ходов до ничьей
stalemateStepsLimit :: Int
stalemateStepsLimit = 15
