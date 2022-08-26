module Player where

data Player = White | Black

playerMessage :: Player -> String
playerMessage White = "White's turn"
playerMessage Black = "Black's turn"

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White
