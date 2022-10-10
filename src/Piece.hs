module Piece where

import Data.List (find)
import Player

type Pos = (Int, Int)
data UnitType = King | Reg deriving (Eq, Show)
data Piece = Piece {
    pos :: Pos,
    player :: Player,
    unitType :: UnitType
} deriving (Eq, Show)

boardMin :: Int
boardMin = 0
boardMax :: Int
boardMax = 10

cornerPos = [
    (boardMin, boardMin),
    (boardMin, boardMax),
    (boardMax, boardMin),
    (boardMax, boardMax)]

pieceString :: Maybe Piece -> String
pieceString (Just Piece{unitType=King}) = "K"
pieceString (Just Piece{player=White}) = "W"
pieceString (Just Piece{player=Black}) = "B"
pieceString Nothing = "."

getPiece :: Pos -> [Piece] -> Maybe Piece
getPiece (x,y) = find (\Piece{ pos=(px, py) } -> px == x && py == y)

isRegEnemyPiece :: Pos -> Player -> [Piece] -> Bool
isRegEnemyPiece pos pl pieces =
    isRegEnemyPiece' (getPiece pos pieces) pl

isRegEnemyPiece' :: Maybe Piece -> Player -> Bool
isRegEnemyPiece' (Just p) pl =
    player p /= pl && unitType p /= King
isRegEnemyPiece' Nothing _ =
    False

isFriendlyPiece :: Pos -> Player -> [Piece] -> Bool
isFriendlyPiece pos pl pieces =
    isFriendlyPiece' (getPiece pos pieces) pl

isFriendlyPiece' :: Maybe Piece -> Player -> Bool
isFriendlyPiece' (Just p) pl =
    player p == pl
isFriendlyPiece' Nothing _ =
    False

initBlack :: [Piece]
initBlack =
    concat [
        map (\i -> Piece { pos=(i,0), player=Black, unitType=Reg }) [3..7],
        map (\i -> Piece { pos=(i,10), player=Black, unitType=Reg }) [3..7],
        map (\i -> Piece { pos=(0,i), player=Black, unitType=Reg }) [3..7],
        map (\i -> Piece { pos=(10,i), player=Black, unitType=Reg }) [3..7],
        map (\(x,y) -> Piece { pos=(x,y), player=Black, unitType=Reg }) [(5,1),(5,9),(1,5),(9,5)]
    ]

initWhite :: [Piece]
initWhite =
    concat [
        map (\i -> Piece { pos=(i,4), player=White, unitType=Reg }) [4..6],
        map (\i -> Piece { pos=(i,6), player=White, unitType=Reg }) [4..6],
        map (\i -> Piece { pos=(i,5), player=White, unitType=Reg }) [3,4],
        map (\i -> Piece { pos=(i,5), player=White, unitType=Reg }) [6,7],
        map (\(x,y) -> Piece { pos=(x,y), player=White, unitType=Reg }) [(5,7),(5,3)],
        [Piece { pos=(5,5), player=White, unitType=King }]
    ]

initPieces :: [Piece]
initPieces = initBlack ++ initWhite

displayRow :: Int -> [Piece] -> String
displayRow r ps =
    foldl (\acc c -> acc ++ pieceString (getPiece (c,r) ps) ++ " ") "" [boardMin..boardMax]

displayBoard :: [Piece] -> String
displayBoard ps =
    foldl (\acc r -> acc ++ displayRow r ps ++ "\n") "" [boardMin..boardMax]

getAdjacentPos :: Piece -> [Pos]
getAdjacentPos Piece{pos=(x,y)} =
    [
        (x+1,y),
        (x-1,y),
        (x,y+1),
        (x,y-1)
    ]

getAdjacentPieces :: Piece -> [Piece] -> [Piece]
getAdjacentPieces p =
    filter (\Piece{pos=pos} -> pos `elem` adjPos)
    where
        adjPos = getAdjacentPos p

getAdjacentEnemies :: Piece -> [Piece] -> [Piece]
getAdjacentEnemies p ps =
    filter (\Piece{player=pl} -> pl /= player p) adjPieces
    where
        adjPieces = getAdjacentPieces p ps

getKing :: [Piece] -> Maybe Piece
getKing = find (\Piece{unitType=ut} -> ut == King)

atEdge :: Piece -> Bool
atEdge Piece{pos=(x,y)} =
    x == boardMin ||
    x == boardMax ||
    y == boardMin ||
    y == boardMax
