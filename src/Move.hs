module Move where

import Piece
import GameState
import Player

type Move = (Pos, Pos)

makeMove :: GameState -> Move -> [Piece]
makeMove gs mv =
    makeMove' (getPiece (fst mv) (pieces gs)) gs mv

makeMove' :: Maybe Piece -> GameState -> Move -> [Piece]
makeMove' (Just srcPiece) gs (src,dest)
    | unitType srcPiece == King = movedPieces
    | otherwise = takePieces dest (currentPlayer gs) movedPieces
    where
        movedPieces = filter (/= srcPiece) (pieces gs) ++ [srcPiece { pos=dest }]
makeMove' Nothing gs _ =
    pieces gs

getAdjacentPos :: Pos -> [(Pos,Pos)]
getAdjacentPos (x,y) =
    [
        ((x-1,y),(x-2,y)),
        ((x+1,y),(x+2,y)),
        ((x,y-1),(x,y-2)),
        ((x,y+1),(x,y+2))
    ]

getAdjacentPieces :: Pos -> [Piece] -> [(Maybe Piece, Maybe Piece)]
getAdjacentPieces pos pieces =
    map (\(p1,p2) -> (getPiece p1 pieces, getPiece p2 pieces)) (getAdjacentPos pos)

filterNothingPieces :: [Piece] -> Maybe Piece -> [Piece]
filterNothingPieces acc (Just p) =
    acc ++ [p]
filterNothingPieces acc Nothing =
    acc

-- TODO: king taking logic
findTakenPos :: Player -> [Piece] -> [Pos] -> (Pos,Pos) -> [Pos]
findTakenPos pl pieces acc (pos1, pos2)
    | isEnemyPiece pos1 pl pieces && pos2 `elem` cornerPos = acc ++ [pos1]
    | isEnemyPiece pos1 pl pieces && isFriendlyPiece pos2 pl pieces = acc ++ [pos1]
    | otherwise = acc

getTakenPieces :: Pos -> Player -> [Piece] -> [Piece]
getTakenPieces pos pl pieces =
    foldl filterNothingPieces [] $ map (`getPiece` pieces) takenPos
    where
        takenPos = foldl (findTakenPos pl pieces) [] (getAdjacentPos pos)

takePieces :: Pos -> Player -> [Piece] -> [Piece]
takePieces dest pl pieces =
    filter (`notElem` takenPieces) pieces
    where
        takenPieces = getTakenPieces dest pl pieces
