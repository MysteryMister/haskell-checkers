module Visuals where

import Graphics.Gloss

import Constants
import Types


--drawing game board
drawBoard :: GameConfig -> Picture
drawBoard (GameConfig _ _ _ _ activeTeam _ _ _ _ _ _ GameOver  _ _) = 
    Translate (-360) 0 (Color red (Text (show activeTeam ++ " lost!")))
drawBoard (GameConfig _ _ _ _ _ _ _ _ _ _ _ Stalemate  _ _) =
    Translate (-350) 0 (Color red (Text "Stalemate!"))
drawBoard (GameConfig 
    fullBoard boardPicture _ _ activeTeam _ Nothing _ _ _ _ _ count _) = 
        Pictures (
            [Scale 0.65 0.65 boardPicture] 
            ++ drawFigures fullBoard 
            ++ drawTeamTurn activeTeam 
            ++ drawSpareCount count)
drawBoard (GameConfig 
    fullBoard boardPicture _ _ activeTeam _ (Just cell) _ _ _ _ _ count _) =
        Pictures (
            [Scale 0.65 0.65 boardPicture] 
            ++ drawFigures fullBoard 
            ++ drawChosenFigure cell 
            ++ drawTeamTurn activeTeam 
            ++ drawSpareCount count)

--drawing figures
drawFigures :: Board -> [Picture]
drawFigures [] = []
drawFigures ((CheckerCell (x, y) (Just (Figure _ img _ _ _)) _) : xs) = 
    Scale 0.3 0.3 (Translate 
        (leftCenter + xOffset * fromIntegral (x-1))  
        (bottomBound + yOffset * fromIntegral (y-1))
        img)
    : drawFigures xs
drawFigures ((CheckerCell _ Nothing _) : xs) = drawFigures xs

--drawing passive turn count
drawSpareCount :: Int -> [Picture]
drawSpareCount count = 
    [Translate 530 0 (Scale 0.5 0.5 (Color blue (Text "Turns before")))]
    ++ [Translate 590 (-100) (Scale 0.5 0.5 (Color blue (Text "stalemate:")))]
    ++ [Translate 700 (-200) (Scale 0.5 0.5 (Color blue (Text (show count))))]

--drawing chosen figure
drawChosenFigure :: CheckerCell -> [Picture]
drawChosenFigure (CheckerCell (x, y) _ _) = 
    [Translate 
        ((-boardRadius + 58) + cellWidth * fromIntegral (x - 1)) 
        ((-boardRadius + 61) + cellWidth * fromIntegral (y - 1)) 
        (Scale 0.8 0.8 (Color green (Circle 55)))]

--drawing active team
drawTeamTurn :: Team -> [Picture]
drawTeamTurn White = Translate (-930) 0 (Color red (Text "White's")) 
    : [Translate (-850) (-200) (Color red (Text "turn"))]
drawTeamTurn Black = Translate (-930) 0 (Color red (Text "Black's")) 
    : [Translate (-850) (-200) (Color red (Text "turn"))]
