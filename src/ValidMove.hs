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

getPosBetween :: Int -> Int -> Int -> [Pos]
getPosBetween srcA destA srcB =
    zip (repeat srcB) (absRange (srcA,destA))

pathIsValid :: Move -> [Piece] -> Bool
pathIsValid ((srcX,srcY),(destX,destY)) ps
    | srcX - destX == 0 = do
        let posBetween = getPosBetween srcY destY srcX
        all (\Piece {pos=pos} -> pos `notElem` posBetween) ps
    | srcY - destY == 0 = do
        let posBetween = getPosBetween srcX destX srcY
        all (\Piece {pos=pos} -> pos `notElem` posBetween) ps
    | otherwise = False

validMove :: GameState -> Move -> Either Error Bool
validMove GameState {pieces=ps, currentPlayer=pl} mv
    | uncurry (==) mv = Left "You must move a unit"
    | not (playerOwnsPiece srcPiece pl) = Left "This is not a unit you can move"
    | not (pathIsValid mv ps) = Left "Units cannot move through other units and must move straight"
    | not (destinationIsValid (snd mv) srcPiece destPiece) = Left "Cannot move this unit to this tile"
    | otherwise = Right True
    where
        srcPiece = getPiece (fst mv) ps
        destPiece = getPiece (snd mv) ps
