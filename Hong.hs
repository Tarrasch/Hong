module Main where

import GameState
import Constants
import PaintGame
import Control.Concurrent
import Graphics.SOE.Gtk
import Control.Monad
import Control.Monad.State
import ButtonControl
import System.Exit
import Data.IORef

----------------- Used Types -----------------

type UCT =  (UserControl, Time)
type MyChannel  = Chan UCT
type MyStateT = StateT ExeState IO

----------------- Data -----------------

data ExeState = ExeState
 {
 window :: Window,
 time0 :: Time,
 timeFactor :: Float,
 timePlus :: Time,
 channel :: MyChannel,
 activeButtons :: ButtonControl
 }

startExeState :: ExeState
startExeState =
  ExeState {
  window = error "You have to initialize the window!" ,
  time0 = error "Bug: forgot to initialize t0",
  timeFactor = 1.0,
  timePlus = 0.0,
  channel = error "Bug: forgot to initialize channel",
  activeButtons = nothingPressedYet
  }

----------------- Functions -----------------

main :: IO()
main = do runStateT initialization startExeState
          return ()

-- The initialization function does quite much, namely:
--
--  * Print friendly game-play instructions
--  * Creates the window, the channel and creates the state
--  * Kicks the playsfunction
initialization :: MyStateT ()
initialization =
        (
            do mapM_ (liftIO . putStrLn)
                ["Welcome to Hong!",
                 "",
                 "Instructions:",
                 "Use w/s and o/l for moving the paddles",
                 "Use p for pause",
                 "Use space-bar for exit",
                 "Use +/- for altering the game speed",
                 "Use r for restart",
                 "You can resize the game as you do with normal windows",
                 "",
                 "Game made by Arash Rouhani (c) 2010" ,
                 ""
                ]

               w <- liftIO $ openWindowEx "Hong!" (Just (0,0)) (Just (xWin,yWin))
                               drawBufferedGraphic Nothing

               chan <- liftIO newChan
               liftIO $ writeChan chan (userControl0, 0.0)
               list <- liftIO $ getChanContents chan

               t0 <- liftIO getTime

               -- Put the window, t0 and the channel into the state
               modify $ \s -> s { window = w, time0 = t0, channel = chan }

               play (unzip list) channelUpdater
            )

-- This function basically:
--
-- 1. Draws the state in the stream (the newest)
-- 2. Call the list-updater function to add another elemnent to the stream
--     and then goes back to point (1) again.
play :: ([UserControl], [Time]) -> MyStateT() -> MyStateT ()
play (ucs, ts) updateList = do
    let allStates = getStates ucs ts
    w <- gets window

    forM_ allStates $ \s -> do liftIO $ drawState w s
                               updateList

-- ChannelUpdater:
--
-- Fills the channel with the new (UserAction, Time) pair
-- by extracting it from the real world.
--
-- The effect becomes that the play-function can recieve
-- yet another state of the game.
channelUpdater :: MyStateT ()
channelUpdater =
  do -- loop :: ButtonControl -> MyStateT ButtonControl
     let loop bc = do w <- gets window
                      mevent <- liftIO $ maybeGetWindowEvent w
                      case mevent of
                       Nothing -> return bc
                       Just e  -> case e of
                                    Closed       -> terminate
                                    Key ' ' True -> terminate
                                    Key 'p' True -> do pauseSequence
                                                       loop bc
                                    Key '+' True -> do changeTimeFlow 1.5
                                                       loop bc
                                    Key '-' True -> do changeTimeFlow (1/1.5)
                                                       loop bc
                                    Key c True   -> loop $ c `enters` bc
                                    Key c False  -> loop $ c `leaves` bc
                                    _            -> loop bc -- We can't handle other kinds of events

     -- Using the loop function like this helps us to not
     -- lookup the buttoncontrol too often
     bc0 <- (gets activeButtons) :: MyStateT ButtonControl
     bc  <- loop bc0

     -- We also save the buttonControl for the next time
     modify $ \s -> s { activeButtons = bc }

     t <- liftIO getTime

     tFactor <- gets timeFactor
     t0 <- gets time0
     tPlus <- gets timePlus

     chan <- gets channel
     liftIO $ writeChan chan (toControls bc, tFactor * (t - t0) + tPlus)


-- We are using Floats throughout this application.
getTime :: IO (Float)
getTime = fmap fromIntegral timeGetTime



----------------- Additional functions -----------------

-- The functions below are not the essential driving-powers for this game,
-- but rather stuff added later on as the game developed to add convenience
-- for the players.


-- Close program.
terminate :: MyStateT a
terminate = do w <- gets window
               liftIO $ putStrLn "Thanks for playing!"
               liftIO $ closeWindow w
               liftIO $ exitSuccess
               return undefined


-- Wait for pause to be repressed and then exit this function
pauseSequence :: MyStateT ()
pauseSequence
  = do timeThen <- liftIO getTime

       -- Start the pausing sequence
       let actualWaiting = do w <- gets window
                              k <- liftIO $ getKey w
                              unless (k=='p' || k == 'P') actualWaiting
       actualWaiting

       -- Ok, pause ended.
       -- Now modify the time-variables so the game continues naturally
       timeNow  <- liftIO getTime
       tFactor <- gets timeFactor
       modify $ \s -> s { timePlus = timePlus s - tFactor*(timeNow-timeThen) }


-- Makes the flow of time go faster/slower.
-- if f > 1 then the games goes faster by the factor f.
changeTimeFlow :: Float -> MyStateT ()
changeTimeFlow f =
      do t0 <- gets time0
         t0' <- liftIO getTime
         modify $ \s -> s { time0 = t0' }
         tFactor <- gets timeFactor
         modify $ \s -> s { timePlus = timePlus s + tFactor * (t0'-t0) }
         modify $ \s -> s { timeFactor = tFactor * f }


