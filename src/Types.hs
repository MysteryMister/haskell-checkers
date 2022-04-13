module Types where

import Graphics.Gloss.Interface.Pure.Game


--team: white and black
data Team = White | Black deriving(Eq, Show)

--figure types: checker and queen
data FigureType = Checker | Queen deriving(Eq, Show)

--figure info:
data Figure = Figure {
    figureType :: FigureType, --type
    image :: Picture, --picture
    figureTeam :: Team, --team
    peaceSteps :: Board, --peaceful steps list
    attackSteps :: Board --attack steps list
} deriving Show

--game state: in progress, game over, stalemate
data GameState = InProgress | GameOver | Stalemate deriving(Eq)

--game configuration: game and turn info
data GameConfig = GameConfig {
    board :: Board, --game board
    boardPic :: Picture, --board picture
    whitePics :: [Picture], --white figures pictures
    blackPics :: [Picture], --black figures pictures
    team :: Team, --active team
    movedFigure :: Maybe CheckerCell, --figure moved during last iteration
    chosenFigure :: Maybe CheckerCell, --chosen figure
    newPosition :: Maybe CheckerCell, --new position for chosen figure
    forcedToAttack :: Bool, --flag: is team forced to attack
    peacefulPositions :: Maybe Board, --chosen figure's peaceful steps
    attackPositions :: Maybe Board, --chosen figure's attacking steps
    gameState :: GameState, --current game state
    checkerSpareSteps :: Int, --turn count without checker aggression
    attackedByChecker :: Bool --flag: did checker attack this turn
}

--checker cell info
data CheckerCell = CheckerCell {
    cellIndex :: (Int, Int), --integer coordinates (example: E4 -> (5, 4))
    figure :: Maybe Figure, --figure on cell
    cellCoordinates :: (Float, Float) --cell's bottom left corner coordinates
} deriving Show

--cell with figure on it
type Cell = (Int, Int, Team)

--game board and figure's steps list
type Board = [CheckerCell]
