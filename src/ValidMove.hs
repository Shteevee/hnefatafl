module ValidMove where

import Piece
import Player
import Move
import GameState

type Error = String

playerOwnsPiece :: Maybe Piece -> Player -> Bool
playerOwnsPiece (Just Piece {player=p}) cp = p == cp
playerOwnsPiece Nothing _ = False

destinationIsValid :: Pos -> Maybe Piece -> Maybe Piece -> Bool
destinationIsValid _ _ (Just destPiece) = False
destinationIsValid dest (Just srcPiece) Nothing
    | dest `elem` cornerPos && unitType srcPiece /= King = False
    | otherwise = True
destinationIsValid _ _ _ = False

absRange :: (Int,Int) -> [Int]
absRange (x,y)
    | x > y = [y..x-1]
    | y > x = [x+1..y]
    | otherwise = []

getPosRangeY :: Int -> Int -> Int -> [Pos]
getPosRangeY srcA destA srcB =
    zip (repeat srcB) (absRange (srcA,destA))

getPosRangeX :: Int -> Int -> Int -> [Pos]
getPosRangeX srcA destA srcB =
    zip (absRange (srcA,destA)) (repeat srcB)

pathIsValid :: Move -> [Piece] -> Bool
pathIsValid ((srcX,srcY),(destX,destY)) pieces
    | srcX - destX == 0 = do
        let posBetween = getPosRangeY srcY destY srcX
        all (\Piece {pos=pos} -> pos `notElem` posBetween) pieces
    | srcY - destY == 0 = do
        let posBetween = getPosRangeX srcX destX srcY
        all (\Piece {pos=pos} -> pos `notElem` posBetween) pieces
    | otherwise = False

validMove :: GameState -> Move -> Maybe Error
validMove GameState {pieces=ps, currentPlayer=pl} mv
    | uncurry (==) mv = Just "You must move a unit"
    | not (playerOwnsPiece srcPiece pl) = Just "This is not a unit you can move"
    | not (pathIsValid mv ps) = Just "Units cannot move through other units and must move straight"
    | not (destinationIsValid (snd mv) srcPiece destPiece) = Just "Cannot move this unit to this tile"
    | otherwise = Nothing
    where
        srcPiece = getPiece (fst mv) ps
        destPiece = getPiece (snd mv) ps
