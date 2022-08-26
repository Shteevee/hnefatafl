module GameState where

import Player
import Piece

data GameState = GameState {
    currentPlayer :: Player,
    pieces :: [Piece]
}

createGameState :: GameState
createGameState = GameState {
    currentPlayer = White,
    pieces = initPieces
}
