module Move where

import Piece
import GameState

type Move = (Pos, Pos)

makeMove :: GameState -> Move -> [Piece]
makeMove gs m =
    if validMove gs m then
        performMove gs m
    else
        pieces gs

validMove :: GameState -> Move -> Bool
validMove _ _ = True

performMove :: GameState -> Move -> [Piece]
performMove gs _ = pieces gs
