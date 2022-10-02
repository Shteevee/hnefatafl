module Player where

data Player = White | Black deriving (Eq, Show)

playerMessage :: Player -> String
playerMessage White = "White's turn"
playerMessage Black = "Black's turn"

victorMessage :: Player -> String
victorMessage White = "White victory!"
victorMessage Black = "Black victory!"

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White
