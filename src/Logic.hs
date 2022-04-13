module Logic where

import Types


--forming peaceful steps list
formPeacefulSteps :: CheckerCell -> Board -> Board
formPeacefulSteps (CheckerCell 
    (i, j) (Just (Figure figType _ figTeam _ _)) _) fullBoard
    | figType == Checker = checkerPeacefulSteps (i, j, figTeam) fullBoard
    | otherwise = queenPeacefulSteps (i, j, figTeam) fullBoard
formPeacefulSteps _ _ = []

--checker's peaceful steps
checkerPeacefulSteps :: Cell -> Board -> Board
checkerPeacefulSteps _ [] = []
checkerPeacefulSteps (i1, j1, t1) ((CheckerCell (i2, j2) Nothing coords) : xs)
    | (i2 == i1 + 1 || i2 == i1 - 1) && j2 == j1 + 1 && t1 == White = 
        checkerCell : checkerPeacefulSteps cell xs
    | (i2 == i1 + 1 || i2 == i1 - 1) && j2 == j1 - 1 && t1 == Black = 
        checkerCell : checkerPeacefulSteps cell xs
    | otherwise = checkerPeacefulSteps cell xs
        where
            checkerCell = CheckerCell (i2, j2) Nothing coords
            cell = (i1, j1, t1)
checkerPeacefulSteps (i1, j1, t1) (_ : xs) = checkerPeacefulSteps cell xs
        where
            cell = (i1, j1, t1)

--queen's peaceful steps: 2 diagonals 
--(i1 - i == j1 - j) and (i1 - i == j - j1)
queenPeacefulSteps :: Cell -> Board -> Board
queenPeacefulSteps (i1, j1, t1) fullBoard = 
    formPeaceDiagonal (i1, j1, t1) 
        (filter 
            (\(CheckerCell (i, j) _ _) -> i1 - i == j1 - j) 
            fullBoard)
    ++ formPeaceDiagonal (i1, j1, t1) 
        (filter 
            (\(CheckerCell (i, j) _ _) -> i1 - i == j - j1) 
            fullBoard)

--queen's peaceful steps from one diagonal: 2 halves
formPeaceDiagonal :: Cell -> Board -> Board
formPeaceDiagonal (i1, j1, _) diagonal = 
    formPeaceHalfDiagonal (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 < i) && (j1 < j)) diagonal)
    ++ formPeaceHalfDiagonal (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 < i) && (j1 > j)) diagonal)
    ++ formPeaceHalfDiagonal (reverse (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 > i) && (j1 > j)) diagonal))
    ++ formPeaceHalfDiagonal (reverse (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 > i) && (j1 < j)) diagonal))

--queen's peaceful steps from one half-diagonal
formPeaceHalfDiagonal :: Board -> Board
formPeaceHalfDiagonal [] = []
formPeaceHalfDiagonal ((CheckerCell (i, j) Nothing (x, y)) : xs) = 
    CheckerCell (i, j) Nothing (x, y) : formPeaceHalfDiagonal xs
formPeaceHalfDiagonal _ = []

--forming attacking steps list
formAttackSteps :: CheckerCell -> Board -> Board
formAttackSteps (CheckerCell 
    (i, j) (Just (Figure figType _ figTeam _ _)) _) fullBoard
    | figType == Checker = checkerAttackSteps (i, j, figTeam) fullBoard
    | otherwise = queenAttackSteps (i, j, figTeam) fullBoard
formAttackSteps _ _ = []

--checker's attacking steps
checkerAttackSteps :: Cell -> Board -> Board
checkerAttackSteps cell fullBoard = 
    createCheckerAttacks cell fullBoard fullBoard

createCheckerAttacks :: Cell -> Board -> Board -> Board
createCheckerAttacks _ [] _ = []
createCheckerAttacks (i1, j1, t1) 
    ((CheckerCell (i2, j2) Nothing coords) : xs) fullBoard
    | i2 == i1 - 2 && j2 == j1 + 2 = 
        if isEnemyOnCell (i1 - 1, j1 + 1) t1 fullBoard 
            then checkerCell : createCheckerAttacks cell xs fullBoard 
            else createCheckerAttacks cell xs fullBoard
    | i2 == i1 + 2 && j2 == j1 + 2 = 
        if isEnemyOnCell (i1 + 1, j1 + 1) t1 fullBoard 
            then checkerCell : createCheckerAttacks cell xs fullBoard 
            else createCheckerAttacks cell xs fullBoard
    | i2 == i1 + 2 && j2 == j1 - 2 = 
        if isEnemyOnCell (i1 + 1, j1 - 1) t1 fullBoard 
            then checkerCell : createCheckerAttacks cell xs fullBoard 
            else createCheckerAttacks cell xs fullBoard
    | i2 == i1 - 2 && j2 == j1 - 2 = 
        if isEnemyOnCell (i1 - 1, j1 - 1) t1 fullBoard 
            then checkerCell : createCheckerAttacks cell xs fullBoard 
            else createCheckerAttacks cell xs fullBoard
    | otherwise = createCheckerAttacks cell xs fullBoard
        where
            checkerCell = CheckerCell (i2, j2) Nothing coords
            cell = (i1, j1, t1)
createCheckerAttacks (i1, j1, t1) (_ : xs) fullBoard = 
    createCheckerAttacks (i1, j1, t1) xs fullBoard

--checking if cell is occupied by enemy
isEnemyOnCell :: (Int, Int) -> Team -> Board -> Bool
isEnemyOnCell _ _ [] = False
isEnemyOnCell (i1, j1) t1 ((CheckerCell _ Nothing _) : xs) = 
    isEnemyOnCell (i1, j1) t1 xs
isEnemyOnCell (i1, j1) t1 ((CheckerCell (i2, j2) (Just fig2) _) : xs) 
    | i1 == i2 && j1 == j2 && figureTeam fig2 /= t1 = True
    | otherwise = isEnemyOnCell (i1, j1) t1 xs

--queen's attacking steps: 2 diagonals 
--(i1 - i == j1 - j) and (i1 - i == j - j1)
queenAttackSteps :: Cell -> Board -> Board
queenAttackSteps (i1, j1, t1) fullBoard = 
    formAttackDiagonal (i1, j1, t1) 
        (filter (\(CheckerCell (i, j) _ _) -> i1 - i == j1 - j) fullBoard)
    ++ formAttackDiagonal (i1, j1, t1) 
        (filter (\(CheckerCell (i, j) _ _) -> i1 - i == j - j1) fullBoard)

--queen's attacking steps from one diagonal: 2 halves
formAttackDiagonal :: Cell -> Board -> Board
formAttackDiagonal (i1, j1, t1) diagonal = 
    formAttackHalfDiagonal t1 (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 < i) && (j1 < j)) diagonal)
    ++ formAttackHalfDiagonal t1 (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 < i) && (j1 > j)) diagonal)
    ++ formAttackHalfDiagonal t1 (reverse (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 > i) && (j1 > j)) diagonal))
    ++ formAttackHalfDiagonal t1 (reverse (filter 
        (\(CheckerCell (i, j) _ _) -> (i1 > i) && (j1 < j)) diagonal))

--queen's attacking steps from half-diagonal
formAttackHalfDiagonal :: Team -> Board -> Board
formAttackHalfDiagonal _ [] = []
formAttackHalfDiagonal t1 ((CheckerCell _ Nothing _) : xs) = 
    formAttackHalfDiagonal t1 xs 
formAttackHalfDiagonal t1 ((CheckerCell _ (Just fig) _) : xs)
    | figureTeam fig == t1 = []
    | otherwise = [head xs | isFreeAndValidFirstCell xs]

--checking if the next cell after figure exists and is free
isFreeAndValidFirstCell :: Board -> Bool
isFreeAndValidFirstCell [] = False
isFreeAndValidFirstCell ((CheckerCell _ Nothing _) : _) = True 
isFreeAndValidFirstCell _ = False
