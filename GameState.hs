module GameState (
    State (..)
  , pong
  , startState
  )
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

pong :: Behavior State
pong = lift4 State lpy rpy bpos bdir
  where lpy = lift0 (leftPaddle  startState) + integral (lift1 playerLeft  uc)
        rpy = lift0 (rightPaddle startState) + integral (lift1 playerRight uc)
        bpos = pairB xpos ypos
        xpos = lift0 ((fst . ballPos) startState) + integral xvel
        ypos = lift0 ((snd . ballPos) startState) + integral yvel
        bdir = pairB xvel yvel
        xvel = (fst . ballDir) startState `stepAccum` xbounce ->> negate
        yvel = (snd . ballDir) startState `stepAccum` ybounce ->> negate
        ldy  = absb $ lpy - ypos
        rdy  = absb $ rpy - ypos
        ldx  = absb $ (-planeHalfWidth) - xpos
        rdx  = absb $ planeHalfWidth    - xpos
        lbounce = when $ ldy <* paddleHalfHeight &&* ldx <* 0.02
        rbounce = when $ rdy <* paddleHalfHeight &&* rdx <* 0.02
        xbounce = lbounce .|. rbounce
        ybounce = when $ absb ypos >* planeHalfWidth
        absb = lift1 abs

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
