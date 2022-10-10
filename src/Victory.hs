module Victory where


import Data.List ( find )
import Player
import GameState
import Piece

checkVictory :: GameState -> Maybe Player
checkVictory gs
    | blackVictory gs = Just Black
    | whiteVictory gs = Just White
    | otherwise = Nothing

whiteVictory :: GameState -> Bool
whiteVictory GameState{ pieces=ps } =
    whiteVictory' $ getKing ps

whiteVictory' :: Maybe Piece -> Bool
whiteVictory' (Just p) =
    pos p `elem` cornerPos
whiteVictory' Nothing =
    False

blackVictory :: GameState -> Bool
blackVictory GameState{ pieces=ps } =
    blackVictory' king ps
    where
        king = getKing ps

blackVictory' :: Maybe Piece -> [Piece] -> Bool
blackVictory' (Just king) ps
    | atEdge king = length adjEnemies == 3
    | otherwise = length adjEnemies == 4
    where
        adjEnemies = getAdjacentEnemies king ps
blackVictory' Nothing _ =
    False
