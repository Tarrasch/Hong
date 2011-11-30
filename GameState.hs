module GameState (
    State (..)
  , pong
  , startState
  , ballPos
  , ballDir
  )
  where
-- | A module declaring the GameState
--   The Module contains the functions that represent the physics the Pong.

import Constants
import UserControl
import Fal

data State =
  State { leftPaddle  :: Float
        , rightPaddle :: Float
        , xPosition   :: Float
        , yPosition   :: Float
        , xVelocity   :: Float
        , yVelocity   :: Float
        }
  deriving Show

ballPos, ballDir :: State -> (Float, Float)
ballPos s = (xPosition s, yPosition s)
ballDir s = (xVelocity s, yVelocity s)

----------------- Examples -----------------

startState = State {
   leftPaddle  = 0
 , rightPaddle = 0
 , xPosition = 0
 , yPosition = 0
 , xVelocity = startSpeedX
 , yVelocity = startSpeedY
}

----------------- State-operations (gameplay) -----------------

pong :: Behavior State
pong = lift6 State lpy rpy xpos ypos xvel yvel
  where lpy = gets leftPaddle  + integral (lift1 playerLeft  uc)
        rpy = gets rightPaddle + integral (lift1 playerRight uc)
        xpos = gets xPosition + integral xvel
        ypos = gets yPosition + integral yvel
        xvel = xVelocity startState `stepAccum` xbounce ->> negate
        yvel = yVelocity startState `stepAccum` ybounce ->> negate
        ldy  = absb $ lpy - ypos
        rdy  = absb $ rpy - ypos
        ldx  = absb $ (-planeHalfWidth) - xpos
        rdx  = absb $ planeHalfWidth    - xpos
        lbounce = when $ ldy <* paddleHalfHeight &&* ldx <* 0.02
        rbounce = when $ rdy <* paddleHalfHeight &&* rdx <* 0.02
        xbounce = lbounce .|. rbounce
        ybounce = when $ absb ypos >* planeHalfWidth
        absb = lift1 abs
        gets f = lift0 (f startState)

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
