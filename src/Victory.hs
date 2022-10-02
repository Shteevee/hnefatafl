module Victory where

import Player
import GameState

checkVictory :: GameState -> Maybe Player
checkVictory gs
    | blackVictory gs = Just Black
    | whiteVictory gs = Just White
    | otherwise = Nothing

whiteVictory :: GameState -> Bool
whiteVictory gs = False

blackVictory :: GameState -> Bool
blackVictory gs = False