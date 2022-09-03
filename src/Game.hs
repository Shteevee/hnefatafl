module Game where

import Control.Monad ( when )
import Player
import Piece
import ParseMove
import Move
import ValidMove
import GameState

gameStep :: GameState -> Maybe Move -> IO ()
gameStep gs (Just move) =
    gameStep' (validMove gs move) gs move  
gameStep gs Nothing = do
    putStrLn "Move was invalid. Moves must be in the format \"a0 k10\""
    handleInput gs

gameStep' :: Either Error Bool -> GameState -> Move -> IO()
gameStep' (Right _) gs move =
    gameLoop gs {
            currentPlayer = nextPlayer $ currentPlayer gs,
            pieces = makeMove gs move
        }
gameStep' (Left err) gs _ = do
    putStrLn err
    handleInput gs

handleInput :: GameState -> IO ()
handleInput gs = do
    line <- getLine
    when (line /= "quit") $ do
        gameStep gs (parseMove line)

gameLoop :: GameState -> IO ()
gameLoop gs = do
    putStrLn $ displayBoard $ pieces gs
    putStrLn $ playerMessage $ currentPlayer gs
    handleInput gs
