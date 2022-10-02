module Game where

import Control.Monad ( when )
import Player
import Piece
import ParseMove
import Move
import ValidMove
import GameState
import Victory

gameStep :: GameState -> Maybe Move -> IO ()
gameStep gs (Just move) =
    gameStep' (validMove gs move) gs move  
gameStep gs Nothing = do
    putStrLn "Move was invalid. Moves must be in the format \"a0 k10\""
    handleInput gs

gameStep' :: Maybe Error -> GameState -> Move -> IO()
gameStep' Nothing gs move =
    gameLoop gs {
            currentPlayer = nextPlayer $ currentPlayer gs,
            pieces = makeMove gs move
        }
gameStep' (Just err) gs _ = do
    putStrLn err
    handleInput gs

handleInput :: GameState -> IO ()
handleInput gs = do
    line <- getLine
    when (line /= "quit") $ do
        gameStep gs (parseMove line)

gameLoop :: GameState -> IO ()
gameLoop gs =
    gameLoop' gs (checkVictory gs)

gameLoop' :: GameState -> Maybe Player -> IO ()
gameLoop' gs Nothing = do
    putStrLn $ displayBoard $ pieces gs
    putStrLn $ playerMessage $ currentPlayer gs
    handleInput gs
gameLoop' gs (Just victor) =
    putStrLn $ victorMessage victor
