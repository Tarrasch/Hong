module GameState
 (State (..),
  startState,
  getStates,
  Time)
  where
-- | A module declaring the GameState
--   The Module contains the functions that represent the physics the Pong.

import Constants
import UserControl
import Fal

data State =
  State { leftPaddle  :: Float,
          rightPaddle :: Float,
          ballPos     :: (Float, Float),
          ballDir     :: (Float, Float)
        }
  deriving Show

----------------- Examples -----------------

startState = State {
 leftPaddle  = 0,
 rightPaddle = 0,
 ballPos     = (0, 0),
 ballDir     = startSpeedXY
}


----------------- State-operations (gameplay) -----------------


-- The important function, using incremental sampling
-- UNIMPLEMENTED! (just starting with Paddles movement)
getStates :: [UserControl] -> [Time] -> [State]
getStates ucs ts = states
  where states = startState : ballsRedirected
        ballsRedirected = map redirectBall statesWithPaddles
        statesWithPaddles = [ s { leftPaddle = l, rightPaddle = r}
                            | (l, r, s) <- zip3 leftPaddles rightPaddles statesWithMovedBall]
        statesWithMovedBall = let zipMove = zipWith moveBall
                              in  dts `zipMove` states
        leftPaddles  = paddlePosition (map playerLeft ucs)  dts
        rightPaddles = paddlePosition (map playerRight ucs) dts
        dts = zipWith (-) (tail ts) ts


-- Given how a player has controlled the paddle, this returns
-- where the paddle is now. Note how it doesn't matter where
-- the ball is or how the other player has controlled his paddle.
paddlePosition :: [PaddleControl] -> [Time] -> [Float]
paddlePosition pcs dts = pp
  where pp = 0: myAdd (pcs `zip` dts) pp

-- Helper-function for paddlePosition
myAdd :: [(PaddleControl, Time)] -> [Float] -> [Float]
myAdd ((dir, dt):pcts) (prev:rest) = nextValue : myAdd pcts rest
  where nextValue' = prev + dt * convertDir dir * paddleSpeedCoefficient
        nextValue  = min absHighestPaddlePoint $ max (-absHighestPaddlePoint) nextValue'
        convertDir Nothing     = 0
        convertDir (Just Up)   = 1
        convertDir (Just Down) = -1


-- This function moves the ball
moveBall :: Time -> State -> State
moveBall dt s@(State _ _ (x, y) (dx, dy)) =
  s {
    ballPos = (x + dx*dt, y + dy*dt)
  }

-- This function should only alter the direction of the ball
redirectBall :: State -> State
redirectBall original@(State lp rp (bpx, bpy) (bdirx, bdiry))
  | bpy * signum bdiry > h'               = original { ballDir = (bdirx, -bdiry) }
  | bpx >< (w', w' + 5) && bdirx > 0 &&
    bpy >< (rp - ph, rp + ph)             = let newDy = swingBall (abs $ rp - bpy) bdiry
                                            in  original { ballDir = (-bdirx, newDy) }
  | bpx >< (-w' - 5, -w') && bdirx < 0 &&
    bpy >< (lp - ph, lp + ph)             = let newDy = swingBall (abs $ lp - bpy) bdiry
                                            in  original { ballDir = (-bdirx, newDy) }
  | otherwise = original
 where r    = ballRadius
       w'   = planeHalfWidth - r
       h'   = planeHalfHeight - r
       ph   = paddleHalfHeight
       (><) = \v (a, b) -> v > a && b > v

-- | To add some "action" to the game, and not having a ball with a fully
--   determinable path, we try to "swing" the ball somewhat extra along
--   the y-axis after being hit by a paddle.
--
--   To make the game "fun", we try to never go below the shartvalue in y-speed.
--   It simply doesn't look exciting with a slow ball.
--
--   Additionally, we do try to swing the ball when it's near the edges of
--   the paddle.
swingBall :: ABC -> -- | Difference between ball's middlepoint and paddle (y-axis)
             ABC -> -- | Current speed of ball in yAxis.
             ABC
swingBall dh dy = newDy
 where trySwingValue =  dy * 3 * abs dh / paddleHalfHeight
       newDy = if abs trySwingValue < minimumYVelocity
               then minimumYVelocity * signum dy
               else trySwingValue
