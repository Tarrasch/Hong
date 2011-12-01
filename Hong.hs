module Main where

import Fal
import GameState
import PaintGame
import Middleware

main :: IO()
main = do mapM_ putStrLn
           [ "Welcome to Hong!"
           , ""
           , "Instructions:"
           , "Use w/s and o/l for moving the paddles"
           , "Use P for pause"
           , "Use space-bar for exit"
           , "Use +/- for altering the game speed"
           , "Use B for make time go backwards"
           , ""
           , "Game made by Arash Rouhani (c) 2011"
           , ""
           ]
          let game = timeMiddleware pong
          repictimate "Hong!" (lift1 statePic game)
