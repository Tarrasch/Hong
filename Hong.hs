module Main where

import Fal

main :: IO()
main = do mapM_ putStrLn
           [ "Welcome to Hong!"
           , ""
           , "Instructions:"
           , "Use w/s and o/l for moving the paddles"
           , "Use p for pause"
           , "Use space-bar for exit"
           , "Use +/- for altering the game speed"
           , "Use r for restart"
           , "You can resize the game as you do with normal windows"
           , ""
           , "Game made by Arash Rouhani (c) 2010"
           , ""
           ]
          repictimate "Hong!" undefined
