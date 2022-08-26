import Game
import GameState

mainCommandLoop :: IO ()
mainCommandLoop = do
    cmd <- getLine
    case cmd of
        "start" -> gameLoop createGameState
        "quit" -> return ()
        _ -> mainCommandLoop

main :: IO ()
main = do
    putStrLn "Welcome to hnefatafl\nType start to start a game or quit to leave"
    mainCommandLoop
