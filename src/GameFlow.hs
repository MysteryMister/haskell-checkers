module GameFlow where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import ConfigBoard
import Constants
import Types


--finishing team's turn
finishTurn :: GameConfig -> GameConfig
finishTurn config
    | team config == White = config {
        team = Black,
        movedFigure = Nothing,
        forcedToAttack = isNotEmptyAttackSteps Black (board config),
        gameState = gameState (manageGameState (manageSpareSteps config1)),
        checkerSpareSteps = checkerSpareSteps (manageSpareSteps config),
        attackedByChecker = False
    }
    | otherwise = config {
        team = White,
        movedFigure = Nothing,
        forcedToAttack = isNotEmptyAttackSteps White (board config),
        gameState = gameState (manageGameState (manageSpareSteps config2)),
        checkerSpareSteps = checkerSpareSteps (manageSpareSteps config),
        attackedByChecker = False
    }
    where
        config1 = config {team = Black}
        config2 = config {team = White}

--updating game state in config: checking for defeat/stalemate
manageGameState :: GameConfig -> GameConfig
manageGameState config = checkDefeat (checkStalemate config)

--checking for team's defeat:
--1) team doesn't have any figures
--2) both team's steps lists are empty
checkDefeat :: GameConfig -> GameConfig
checkDefeat config 
    | null (getTeamFigures (team config) (board config)) 
        || (not (isNotEmptyAttackSteps (team config) (board config)) 
        && not (isNotEmptyPeacefulSteps (team config) (board config))) =
            config {gameState = GameOver}
    | otherwise = config

--getting all cells with team's figures on them
getTeamFigures :: Team -> Board -> Board 
getTeamFigures _ [] = []
getTeamFigures t1 ((CheckerCell (i, j) Nothing (x, y)) : xs) =
    getTeamFigures t1 xs
getTeamFigures t1 ((CheckerCell (i, j) 
    (Just (Figure figType img t peaces attacks)) (x, y)) : xs)
    | t == t1 = cell : getTeamFigures t1 xs
    | otherwise = getTeamFigures t1 xs
    where 
        cell = CheckerCell (i, j) 
            (Just (Figure figType img t peaces attacks)) (x, y)

--checking for stalemate
checkStalemate :: GameConfig -> GameConfig
checkStalemate config 
    | checkerSpareSteps config == 0 = config {gameState = Stalemate}
    | otherwise = config

--updating passive steps count in config
manageSpareSteps :: GameConfig -> GameConfig
manageSpareSteps config
    | attackedByChecker config = config {
        checkerSpareSteps = stalemateStepsLimit 
    }
    | otherwise = config {
        checkerSpareSteps = checkerSpareSteps config - 1
    } 

--finishing one iteration
finishIteration :: GameConfig -> GameConfig 
finishIteration 
    (GameConfig 
        board boardImg whites blacks team 
        movedFig (Just chosenFig) (Just newPos) 
        isForced peaces attacks state countLeft attackedChecker) = 
            if attackedChecker then
                GameConfig 
                    newBoard boardImg whites blacks team 
                    (getCell (cellIndex newPos) newBoard) Nothing Nothing 
                    (isNotEmptyAttackSteps team newBoard) 
                    Nothing Nothing state countLeft
                    attackedChecker
            else        
                GameConfig 
                    newBoard boardImg whites blacks team 
                    (getCell (cellIndex newPos) newBoard) Nothing Nothing 
                    (isNotEmptyAttackSteps team newBoard) 
                    Nothing Nothing state countLeft
                    (isAttackStepbyChecker (figure chosenFig) newPos)
            where
                newBoard = formAllSteps 
                    (updateBoard chosenFig newPos board config)
                config = 
                    GameConfig 
                        board boardImg whites blacks team 
                        movedFig (Just chosenFig) (Just newPos) 
                        isForced peaces attacks state 
                        countLeft attackedChecker
finishIteration config = config

--checking, if checker attacked
isAttackStepbyChecker :: Maybe Figure -> CheckerCell -> Bool
isAttackStepbyChecker Nothing _ = False 
isAttackStepbyChecker (Just (Figure figType _ _ _ attacks)) cell
    | figType == Checker && isCellInPossiblePositions cell attacks = True 
    | otherwise = False

--updating board (moving figures)
updateBoard :: CheckerCell -> CheckerCell -> Board -> GameConfig -> Board
updateBoard 
    (CheckerCell 
        (i1, j1) 
        (Just (Figure Checker img White peaces attacks)) 
        (x1, y1)) 
    cell2 board config
    | snd (cellIndex cell2) == 8 = checkerToQueen 
        (CheckerCell 
            (i1, j1) 
            (Just (Figure Checker img White peaces attacks)) 
            (x1, y1))
        cell2 
        (getPictureByIndex 2 (whitePics config))
        White 
        board
updateBoard 
    (CheckerCell 
        (i1, j1) 
        (Just (Figure Checker img Black peaces attacks)) 
        (x1, y1)) 
    cell2 board config
    | snd (cellIndex cell2) == 1 = checkerToQueen 
        (CheckerCell 
            (i1, j1) 
            (Just (Figure Checker img Black peaces attacks)) 
            (x1, y1))
        cell2 
        (getPictureByIndex 2 (blackPics config))
        Black 
        board      
updateBoard cell1 cell2 board _ = makeStep cell1 cell2 board

--getting figure picture by its index from config
getPictureByIndex :: Int -> [Picture] -> Picture
getPictureByIndex 1 (img : xs) = img
getPictureByIndex n (img : xs) = getPictureByIndex (n - 1) xs
getPictureByIndex _ [] =  
    Translate 530 (-300) (Scale 0.3 0.3 (Color red (Text "Image not found!")))

--changing checker to queen
checkerToQueen :: CheckerCell -> CheckerCell -> Picture -> 
    Team -> Board -> Board
checkerToQueen _ _ _ _ [] = []
checkerToQueen cell1 cell2 img team (cell : xs)
    | cellIndex cell == cellIndex cell1 = 
        CheckerCell 
            (cellIndex cell) 
            Nothing 
            (cellCoordinates cell) 
        : checkerToQueen cell1 cell2 img team xs
    | cellIndex cell == cellIndex cell2 = 
        CheckerCell 
            (cellIndex cell) 
            (Just (Figure Queen img team [] [])) 
            (cellCoordinates cell) 
        : checkerToQueen cell1 cell2 img team xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) >= 2 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) >= 2 =
            if fst (cellIndex cell) == (fst (cellIndex cell2) + 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) + 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : checkerToQueen cell1 cell2 img team xs
            else
                cell : checkerToQueen cell1 cell2 img team xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) >= 2 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) <= (-2) =
            if fst (cellIndex cell) == (fst (cellIndex cell2) + 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) - 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : checkerToQueen cell1 cell2 img team xs
            else
                cell : checkerToQueen cell1 cell2 img team xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) <= (-2) 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) >= 2 =
            if fst (cellIndex cell) == (fst (cellIndex cell2) - 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) + 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : checkerToQueen cell1 cell2 img team xs
            else
                cell : checkerToQueen cell1 cell2 img team xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) <= (-2) 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) <= (-2) =
            if fst (cellIndex cell) == (fst (cellIndex cell2) - 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) - 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : checkerToQueen cell1 cell2 img team xs
            else
                cell : checkerToQueen cell1 cell2 img team xs
    | otherwise  = cell : checkerToQueen cell1 cell2 img team xs

--moving figure (== 1 iteration)
makeStep :: CheckerCell -> CheckerCell -> Board -> Board 
makeStep _ _ [] = []
makeStep cell1 cell2 (cell : xs)
    | cellIndex cell == cellIndex cell1 = 
        CheckerCell 
            (cellIndex cell) 
            Nothing 
            (cellCoordinates cell) 
        : makeStep cell1 cell2 xs
    | cellIndex cell == cellIndex cell2 = 
        CheckerCell 
            (cellIndex cell) 
            (figure cell1) 
            (cellCoordinates cell) 
        : makeStep cell1 cell2 xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) >= 2 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) >= 2 =
            if fst (cellIndex cell) == (fst (cellIndex cell2) + 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) + 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : makeStep cell1 cell2 xs
            else
                cell : makeStep cell1 cell2 xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) >= 2 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) <= (-2) =
            if fst (cellIndex cell) == (fst (cellIndex cell2) + 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) - 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : makeStep cell1 cell2 xs
            else
                cell : makeStep cell1 cell2 xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) <= (-2) 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) >= 2 =
            if fst (cellIndex cell) == (fst (cellIndex cell2) - 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) + 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : makeStep cell1 cell2 xs
            else
                cell : makeStep cell1 cell2 xs
    | (fst (cellIndex cell1) - fst (cellIndex cell2)) <= (-2) 
        && (snd (cellIndex cell1) - snd (cellIndex cell2)) <= (-2) =
            if fst (cellIndex cell) == (fst (cellIndex cell2) - 1) 
                && snd (cellIndex cell) == (snd (cellIndex cell2) - 1) 
            then 
                CheckerCell 
                    (cellIndex cell) 
                    Nothing 
                    (cellCoordinates cell) 
                : makeStep cell1 cell2 xs
            else
                cell : makeStep cell1 cell2 xs
    | otherwise  = cell : makeStep cell1 cell2 xs
