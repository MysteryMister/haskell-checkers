module Types where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss


-----Типы данных------

--команда
data Team = White | Black deriving(Eq, Show)

--классы фигур: простая шашка и дамка
data FigureType = Checker | Queen deriving(Eq, Show)

--фигура имеет след. свойства:
--1) тип фигуры
--2) картинка фигуры
--3) команда
--4) список 'мирных' ходов фигуры
--5) список атакующих ходов фигуры
data Figure = Figure FigureType Picture Team Board Board deriving Show

--состояние игры: в процессе, закончена, ничья
data GameState = InProgress | GameOver | Stalemate deriving(Eq)

--конфиг игры: содержит информацию об игре в целом и текущем ходе
data GameConfig = GameConfig {
    board :: Board, --доска
    boardPic :: Picture, --картинка игровой доски
    whitePics :: [Picture], --картинки белых фигур
    blackPics :: [Picture], --картинки чёрных фигур
    team :: Team, --активная команда
    movedFigure :: Maybe Figure, --двигавшаяся на прошлой итерации фигура
    chosenFigure :: Maybe Figure, --выбранная фигура
    newPosition :: Maybe CheckerCell, --новая клетка выбранной фигуры
    forcedToAttack :: Bool, --флаг: вынуждена ли команда атаковать
    peacefulPositions :: Board, --список 'мирных' ходов выбранной фигуры
    attackPositions :: Board, --список атакующих ходов выбранной фигуры
    gameState :: GameState, --текущее состояние игры
    checkerSpareSteps :: Int --число ходов подряд, где не было взятия шашкой
}

--шашечная клетка:
--1) задается целыми координатами (например, E4 -> (5, 4))
--2) может иметь на себе фигуру
--3) координаты левого нижнего угла используются для отрисовки
type CheckerCell = (Int, Int, Maybe Figure, Float, Float)

--клетка с фигурой
type Cell = (Int, Int, Team)

--доска и список ходов конкретной фигуры
type Board = [CheckerCell]
