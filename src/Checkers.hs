module Checkers where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import ConfigBoard
import Constants
import GameFlow
import Logic
import Types
import Visuals


--game initialization, loading figures and board pictures
run :: IO ()
run = do
    boardPicture <- loadBMP "src/board.bmp"
    bCheckerPicture <- loadBMP "src/bChecker.bmp"
    bQueenPicture <- loadBMP "src/bQueen.bmp"
    wCheckerPicture <- loadBMP "src/wChecker.bmp"
    wQueenPicture <- loadBMP "src/wQueen.bmp"
    play 
        vision bgColor fps 
        (GameConfig 
            (createBoard 
                [wCheckerPicture, wQueenPicture] 
                [bCheckerPicture, bQueenPicture] 
                [CheckerCell 
                    (x, y) Nothing 
                    (
                        leftBound + xOffset * fromIntegral (x - 1), 
                        bottomBound + yOffset * fromIntegral (y - 1)
                    ) 
                    | x <- shape, y <- shape]) 
            boardPicture 
            [wCheckerPicture, wQueenPicture] [bCheckerPicture, bQueenPicture]
            White Nothing Nothing Nothing False 
            Nothing Nothing InProgress stalemateStepsLimit False) 
        drawBoard handleEvent update

--per frame update (not used)
update :: Float -> GameConfig -> GameConfig
update _ config = config

--handling events (clicks)
handleEvent :: Event -> GameConfig -> GameConfig
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) config
    | gameState config == InProgress = updateConfig (checkCoordinates (x, y) config)
    | otherwise = config
handleEvent (EventKey (SpecialKey KeySpace) Down _ (x, y)) config = restart config
handleEvent _ config = config

--config update
updateConfig :: GameConfig -> GameConfig 
updateConfig config 
    | isJust (newPosition config) && isCellInPossiblePositions 
        (fromJust (newPosition config)) 
        (fromJust (peacefulPositions config)) 
        = finishTurn (finishIteration config)
    | isJust (newPosition config) 
        && isCellInPossiblePositions 
            (fromJust (newPosition config)) 
            (fromJust (attackPositions config))
        && isJust nextAttacks
        && null (fromJust nextAttacks)
        = finishTurn (finishIteration config)
    | isJust (movedFigure config) && isJust (newPosition config) = 
        finishIteration config
    | isJust (newPosition config) = finishIteration config
    | otherwise = config
        where 
            nextAttacks = getCellAttackSteps 
                (cellIndex (fromJust (newPosition config)))
                newBoard
            newBoard = formAllSteps (updateBoard 
                (fromJust (chosenFigure config)) 
                (fromJust (newPosition config)) 
                (board config)
                config)

--config restart
restart :: GameConfig -> GameConfig
restart config = GameConfig 
    (createBoard 
        (whitePics config) (blackPics config) 
        [CheckerCell 
            (x, y) Nothing 
            (
                leftBound + xOffset * fromIntegral (x - 1), 
                bottomBound + yOffset * fromIntegral (y - 1)
            ) 
            | x <- shape, y <- shape]) 
    (boardPic config) (whitePics config) (blackPics config) 
    White Nothing Nothing Nothing False 
    Nothing Nothing InProgress stalemateStepsLimit False

--checking if player clicked particular cell
checkCoordinates :: (Float, Float) -> GameConfig -> GameConfig 
checkCoordinates (x, y) config
    | x > boardRadius || x < (-boardRadius) 
        || y > boardRadius || y < (-boardRadius) = config
    | isNothing (movedFigure config) && isNothing (chosenFigure config) = 
        if forcedToAttack config then
            changeChosenFigure 
                (getAttackerCell 
                    (coordinatesToCellIndex (x, y)) 
                    (board config)) 
                config
        else
            changeChosenFigure 
                (getCell (coordinatesToCellIndex (x, y)) (board config)) 
                config
    | isNothing (movedFigure config) && isJust (chosenFigure config) = 
        if forcedToAttack config then
            if isCellInPossiblePositions cell (fromJust attacks) then
                config {newPosition = Just cell}
            else 
                changeChosenFigure 
                    (getAttackerCell 
                        (coordinatesToCellIndex (x, y)) 
                        (board config)) 
                    config
        else
            if isCellInPossiblePositions cell (fromJust peaces) then
                config {newPosition = Just cell}
            else 
                changeChosenFigure 
                    (getCell 
                        (coordinatesToCellIndex (x, y)) 
                        (board config)) 
                    config
    | otherwise = 
        if isCellInPossiblePositions cell (fromJust newAttacks) then
            newConfig {newPosition = Just cell}
        else newConfig 
        where 
            newConfig = changeChosenFigure (movedFigure config) config
            cell = fromJust (getCell 
                (coordinatesToCellIndex (x, y)) 
                (board config))
            newAttacks = attackPositions newConfig
            attacks = attackPositions config
            peaces = peacefulPositions config
 
--translating float coordinates to cell index
coordinatesToCellIndex :: (Float, Float) -> (Int, Int)
coordinatesToCellIndex (x, y) = 
    (truncate ((x + boardRadius) / cellWidth) + 1, 
    truncate ((y + boardRadius) / cellWidth) + 1)

--updating chosen figure in config
changeChosenFigure :: Maybe CheckerCell -> GameConfig -> GameConfig
changeChosenFigure Nothing config = config
changeChosenFigure (Just cell) config = if clickOwnCell cell config
    then (config {
        chosenFigure = Just cell,
        peacefulPositions = Just (formPeacefulSteps cell (board config)),
        attackPositions = Just (formAttackSteps cell (board config))
    })
    else config

--checking if clicked on your own cell
clickOwnCell :: CheckerCell -> GameConfig  -> Bool
clickOwnCell (CheckerCell _ Nothing _) _ = False
clickOwnCell (CheckerCell _ (Just figure) _) config = 
    figureTeam figure == team config

--getting attacking cell by its index
getAttackerCell :: (Int, Int) -> Board -> Maybe CheckerCell
getAttackerCell _ [] = Nothing 
getAttackerCell coords (cell : xs)
    | cellIndex cell == coords && isJust (figure cell) = 
        if null (attackSteps (fromJust (figure cell))) then Nothing 
        else Just cell 
    | otherwise = getAttackerCell coords xs

--getting cell by its index if this cell has figure on it
getCellWithFigure :: (Int, Int) -> Board -> Maybe CheckerCell
getCellWithFigure _ [] = Nothing
getCellWithFigure coords (cell : xs)
    | cellIndex cell == coords && isJust (figure cell) =
        Just cell
    | otherwise = getCellWithFigure coords xs
