module ConfigBoard where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Constants
import Logic
import Types


--creating board: placing figures at starting positions
createBoard :: [Picture] -> [Picture] -> Board -> Board
createBoard p1 p2 [] = []
createBoard 
    [wChecker, wQueen] 
    [bChecker, bQueen] 
    ((CheckerCell (i, j) fig (fi, fj)):xs)
        | (j == 1 || j == 3) && odd i || j == 2 && even i = CheckerCell 
            (i, j) 
            (Just (Figure Checker wChecker White [] [])) 
            (fi, fj) 
            : createBoard p1 p2 xs
        | (j == 6 || j == 8) && even i || j == 7 && odd i = CheckerCell 
            (i, j) 
            (Just (Figure Checker bChecker Black [] [])) 
            (fi, fj) 
            : createBoard p1 p2 xs
        | otherwise = CheckerCell (i, j) fig (fi, fj) : createBoard p1 p2 xs
        where 
            p1 = [wChecker, wQueen]
            p2 = [bChecker, bQueen]
createBoard p1 p2 lst = lst

--forming steps lists for all figures on board
formAllSteps :: Board -> Board
formAllSteps board = formTeamSteps (formTeamSteps board White) Black

--forming team's steps lists
formTeamSteps :: Board -> Team -> Board
formTeamSteps board team = teamSteps team board board

teamSteps :: Team -> Board -> Board -> Board
teamSteps _ _ [] = []
teamSteps t1 fullBoard ((CheckerCell (i, j) Nothing (x, y)) : xs) = 
    CheckerCell (i, j) Nothing (x, y) : teamSteps t1 fullBoard xs
teamSteps t1 fullBoard ((CheckerCell (i, j) 
    (Just (Figure figType img t peaces attacks)) (x, y)) : xs)
    | t == t1 = CheckerCell (i, j) 
        (Just (Figure figType img t 
            (formPeacefulSteps cell fullBoard) 
            (formAttackSteps cell fullBoard))) 
        (x, y) : teamSteps t1 fullBoard xs
    | otherwise = cell : teamSteps t1 fullBoard xs
        where
            cell = CheckerCell (i, j) 
                (Just (Figure figType img t peaces attacks)) (x, y)

--checking if cell is in chosen figure's steps list
isCellInPossiblePositions :: CheckerCell -> Board -> Bool 
isCellInPossiblePositions _ [] = False
isCellInPossiblePositions (CheckerCell (i1, j1) fig (x, y)) 
    ((CheckerCell (i, j) _ _) : xs)
    | i == i1 && j == j1 = True 
    | otherwise = isCellInPossiblePositions cell xs
        where
            cell = CheckerCell (i1, j1) fig (x, y)

--checking if team's attacking steps list is not empty
isNotEmptyAttackSteps :: Team -> Board -> Bool 
isNotEmptyAttackSteps _ [] = False
isNotEmptyAttackSteps t1 ((CheckerCell _ Nothing _) : xs) = 
    isNotEmptyAttackSteps t1 xs
isNotEmptyAttackSteps t1 
    ((CheckerCell _ (Just (Figure _ _ t _ attacks)) _) : xs)
    | t == t1 = not (null attacks) || isNotEmptyAttackSteps t1 xs
    | otherwise = isNotEmptyAttackSteps t1 xs

--checking if team's peaceful steps list is not empty
isNotEmptyPeacefulSteps :: Team -> Board -> Bool 
isNotEmptyPeacefulSteps _ [] = False
isNotEmptyPeacefulSteps t1 ((CheckerCell _ Nothing _) : xs) = 
    isNotEmptyPeacefulSteps t1 xs
isNotEmptyPeacefulSteps t1 
    ((CheckerCell _ (Just (Figure _ _ t peaces _)) _) : xs)
    | t == t1 = not (null peaces) || isNotEmptyPeacefulSteps t1 xs
    | otherwise = isNotEmptyPeacefulSteps t1 xs

--getting cell's attacking steps list
getCellAttackSteps :: (Int, Int) -> Board -> Maybe Board
getCellAttackSteps _ [] = Nothing
getCellAttackSteps (i1, j1) ((CheckerCell _ Nothing _) : xs) =
    getCellAttackSteps (i1, j1) xs
getCellAttackSteps (i1, j1)  
    ((CheckerCell index (Just (Figure _ _ _ _ attacks)) _) : xs) 
    | index == (i1, j1) = Just attacks
    | otherwise = getCellAttackSteps (i1, j1) xs

--getting cell by its index 
getCell :: (Int, Int) -> Board -> Maybe CheckerCell
getCell _ [] = Nothing
getCell coords (cell : xs) 
    | cellIndex cell == coords = Just cell
    | otherwise = getCell coords xs
