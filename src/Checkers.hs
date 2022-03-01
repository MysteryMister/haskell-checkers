module Checkers where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Constants
import Types


--основные правила игры в шашки--
--1) игра пошаговая
--2) ходить можно только по диагоналям
--3) 2 типа фигур: шашка и дамка
--4) шашка может ходить только на 1 клетку (на 2 при взятии)
--5) дамка может ходить на любое кол-во клеток
--6) если произошло взятие, походившая фигура обязана продолжать ходить
--до тех пор, пока у нее есть возможность взятия других фигур оппонента
--7) при достижении шашкой последней линии доски она превращается в
--дамку и, если попала туда через взятие, продолжает ходить уже как дамка
--8) игра заканчивается либо поражением одной команды, либо ничьей
--9) поражение в 2 случаях:
----i) закончились фигуры
----ii) закончились возможные ходы
--10) ничья в 1 случае: в течение 15 ходов (ход != итерация)
--ни одна из сторон не делала взятия шашкой 
---------------------------------


----функции----

--загрузка картинок фигур и доски, инициализация игры
run :: IO ()
run = do
    print "Hello!"

--обработка событий (нажатий)
handleEvent :: Event -> GameConfig -> GameConfig
handleEvent _ config = config

--обновление конфига
updateConfig :: CheckerCell -> GameConfig -> GameConfig 
updateConfig _ config = config

--перевод координат в номер клетки
coordinatesToCellIndex :: (Float, Float) -> (Int, Int)
coordinatesToCellIndex _ = (0, 0)

--проверка, нажал ли игрок на определённую клетку 
checkCoordinates :: (Float, Float) -> GameConfig -> GameConfig 
checkCoordinates _ config = config

--получение клетки по её индексу на доске
getCell :: (Int, Int) -> Board -> Maybe CheckerCell
getCell _ board = Nothing

--проверка клика по собственной фигуре
clickOwnCell :: Maybe CheckerCell -> GameConfig  -> Bool
clickOwnCell _ config = False

--на клетке:
--1) нет фигуры - конфиг не изменяется
--2) есть фигура - проверяем её цвет
checkFigureOnCell :: Maybe CheckerCell -> GameConfig -> GameConfig
checkFigureOnCell _ config = config

--нажатие на:
--1) фигуру оппонента - конфиг не изменяется
--2) свою фигуру - конфиг обновляется
checkColor :: CheckerCell -> GameConfig -> GameConfig 
checkColor _ config = config

--замена фигуры chosenFigure в конфиге
changeChosenFigure :: Maybe CheckerCell -> GameConfig -> GameConfig
changeChosenFigure _ config = config

--получение клетки с фигурой
getFigureFromCell :: (Int, Int) -> Board -> Maybe CheckerCell
getFigureFromCell _ board = Nothing

--проверка, есть ли клетка в списке возможных ходов 
findCellInPossiblePositions :: Maybe CheckerCell -> Maybe Board -> Bool 
findCellInPossiblePositions _ board = False


--формирование списка 'мирных' ходов
formPeacefulSteps :: CheckerCell -> Board -> Board
formPeacefulSteps _ board = board

--'мирные' ходы шашки
checkerPeacefulSteps :: CheckerCell -> Board -> Board
checkerPeacefulSteps _ board = board

--'мирные' ходы дамки
queenPeacefulSteps :: CheckerCell -> Board -> Board
queenPeacefulSteps _ board = board

--формирование списка атакующих ходов
formAttackSteps :: CheckerCell -> Board -> Board
formAttackSteps _ board = board

--атакующие ходы шашки
checkerAttackSteps :: CheckerCell -> Board -> Board
checkerAttackSteps _ board = board

--атакующие ходы дамки
queenAttackSteps :: CheckerCell -> Board -> Board
queenAttackSteps _ board = board

--проверка, есть ли на клетке фигура
isOccupiedCell :: (Int, Int) -> Board -> Bool
isOccupiedCell _ board = False

--список мирных ходов на диагонали дамки
formPeaceDiagonal :: CheckerCell -> Board -> Board
formPeaceDiagonal _ board = board

--список атакующих ходов на диагонали дамки
formAttackDiagonal :: CheckerCell -> Board -> Board
formAttackDiagonal _ board = board


--отображение доски
drawBoard :: GameConfig -> Picture
drawBoard config = Translate 0 0 (Color blue (Text "Test sample."))

--отображение фигур
drawFigure :: Board -> [Picture]
drawFigure board = []

--отображение счётчика ходов до ничьей
drawSpareCount :: Int -> Picture
drawSpareCount x = Translate 0 0 (Color blue (Text "Test sample."))

--отображение выбранной фигуры
drawChosenFigure :: CheckerCell -> Picture
drawChosenFigure cell = Translate 0 0 (Color blue (Text "Test sample."))


--инициализация доски
createBoard :: [Picture] -> [Picture] -> Board
createBoard p1 p2 = []

--расстановка фигур по стартовым позициям
createFigures :: [Picture] -> [Picture] -> Board -> Board
createFigures p1 p2 board = board

--формирование списка атакующих ходов команды
formTeamAttackSteps :: Board -> Team -> Board
formTeamAttackSteps board _ = board

--формирование списка мирных ходов команды
formTeamPeacefulSteps :: Board -> Team -> Board
formTeamPeacefulSteps board _ = board

--проверка, не пуст ли список атакующих ходов команды
notEmptyAttackSteps :: Board -> Bool 
notEmptyAttackSteps board = False

--проверка, не пуст ли список мирных ходов команды
notEmptyPeacefulSteps :: Board -> Bool 
notEmptyPeacefulSteps board = False


--завершение хода
finishTurn :: GameConfig -> GameConfig
finishTurn config = config

--завершение итерации
finishIteration :: GameConfig -> GameConfig 
finishIteration config = config

--обновление доски (перемещение фигур)
updateBoard :: CheckerCell -> CheckerCell -> Board -> GameConfig -> Board
updateBoard _ _ board _ = board

--получение картинки фигуры по индексу из конфига
getPictureByIndex :: Int -> [Picture] -> Picture
getPictureByIndex num pictures = Translate 0 0 (Color blue (Text "Test sample."))

--перемещение фигуры (== 1 итерация)
makeStep :: CheckerCell -> CheckerCell -> Board -> Board 
makeStep _ _ board = board

--управление состоянием игры:
--1) проверка условий поражения/ничьей
--2) проверка на пустоту списков атакующих и мирных ходов команды
manageGameState :: GameConfig -> GameConfig
manageGameState config = config

--проверка условий поражения
checkDefeat :: GameConfig -> GameConfig
checkDefeat config = config

--получение всех клеток с фигурами команды
getTeamFigures :: Board -> Team -> Board 
getTeamFigures board _ = board

--проверка условий ничьей
checkStalemate :: GameConfig -> GameConfig
checkStalemate config = config
