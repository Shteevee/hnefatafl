module Piece where

import Player
import Data.List (find)

type Pos = (Int, Int)
data PieceType = King | Reg
data Piece = Piece {
    pos :: Pos,
    player :: Player,
    t :: PieceType
}

boardMin :: Int
boardMin = 0
boardMax :: Int
boardMax = 10

pieceString :: Maybe Piece -> String
pieceString (Just Piece{t=King}) = "K"
pieceString (Just Piece{player=White}) = "W"
pieceString (Just Piece{player=Black}) = "B"
pieceString Nothing = "."

getPiece :: Int -> Int -> [Piece] -> Maybe Piece
getPiece x y = find (\Piece{ pos=(px, py) } -> px == x && py == y)

initBlack :: [Piece]
initBlack =
    concat [
        map (\i -> Piece { pos=(i,0), player=Black, t=Reg }) [3..7],
        map (\i -> Piece { pos=(i,10), player=Black, t=Reg }) [3..7],
        map (\i -> Piece { pos=(0,i), player=Black, t=Reg }) [3..7],
        map (\i -> Piece { pos=(10,i), player=Black, t=Reg }) [3..7],
        map (\(x,y) -> Piece { pos=(x,y), player=Black, t=Reg }) [(5,1),(5,9),(1,5),(9,5)]
    ]

initWhite :: [Piece]
initWhite =
    concat [
        map (\i -> Piece { pos=(i,4), player=White, t=Reg }) [4..6],
        map (\i -> Piece { pos=(i,6), player=White, t=Reg }) [4..6],
        map (\i -> Piece { pos=(i,5), player=White, t=Reg }) [3,4],
        map (\i -> Piece { pos=(i,5), player=White, t=Reg }) [6,7],
        map (\(x,y) -> Piece { pos=(x,y), player=White, t=Reg }) [(5,7),(5,3)],
        [Piece { pos=(5,5), player=White, t=King }]
    ]

initPieces :: [Piece]
initPieces = initBlack ++ initWhite

displayRow :: Int -> [Piece] -> String
displayRow r ps =
    foldl (\acc c -> acc ++ pieceString (getPiece c r ps)) "" [boardMin..boardMax]


displayBoard :: [Piece] -> String
displayBoard ps =
    foldl (\acc r -> acc ++ displayRow r ps ++ "\n") "" [boardMin..boardMax]
