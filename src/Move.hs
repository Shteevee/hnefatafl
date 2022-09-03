module Move where

import Piece
import GameState

type Move = (Pos, Pos)

makeMove :: GameState -> Move -> [Piece]
makeMove gs mv =
    makeMove' (getPiece (fst mv) (pieces gs)) gs mv

makeMove' :: Maybe Piece -> GameState -> Move -> [Piece]
makeMove' (Just srcPiece) gs (src,dest) =
    filter (/= srcPiece) (pieces gs) ++ [srcPiece { pos=dest }]
makeMove' Nothing gs _ =
    pieces gs
